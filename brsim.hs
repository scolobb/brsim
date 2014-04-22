{-# LANGUAGE OverloadedStrings #-}
{- brsim.hs

A Basic Reaction Systems Simulator. -}

import ReactionSystems
import Parser
import Formatter
import qualified System.Console.Argument as Arg
import qualified System.Console.Command as Cmd
import System.Console.Program
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as TextIO
import qualified Data.Set as Set
import System.IO (stdout, hFlush)

-- The possible reaction description formats.
data ReactionFormat = Plain -- A reaction is given as three lists of symbol names.
                    | Arrow -- A reaction is given in a notation similar to the chemical one.
                    deriving (Eq, Ord, Show, Read)

-- Reads the supplied file containing the description of the reaction
-- system and, maybe, the list of contexts.  If a separate context
-- file is specified, the list of context in the reaction system file
-- is ignored.
readInput :: FilePath -> ReactionFormat -> FilePath -> IO (ReactionSystem, [Context])
readInput rsFile format ctxFile = do
  desc <- TextIO.readFile rsFile
  let (txtRs, maybeTxtCtx) = Text.breakOn "\n---" desc

  txtCtx <- if ctxFile /= ""
            then TextIO.readFile ctxFile
            else if Text.null maybeTxtCtx
                 then return Text.empty
                 else return $ Text.drop 4 maybeTxtCtx

  let rules = case format of
        Plain -> readPlainReactions txtRs
        Arrow -> readArrowReactions txtRs

      contexts = readListOfListsOfSymbols txtCtx

  return (makeReactionSystem rules, contexts)

-- Takes care of outputting the results of a simulation.
writeOutput :: ReactionSystem -> InteractiveProcess -> ReactionFormat -> FilePath -> FilePath -> IO ()
writeOutput rs iprocess format outputFile annotationFile = do
  outputFunc $ showListOfListsOfSymbols $ tail $ results iprocess

  if annotationFile /= ""
    then TextIO.writeFile annotationFile $ annotateFunc rs iprocess
    else return ()

  where outputFunc = case outputFile of
          "" -> TextIO.putStr
          file -> TextIO.writeFile file

        annotateFunc = case format of
          Plain -> annotatePlain
          Arrow -> annotateArrow

-- Runs the simulation of the supplied reaction system with the given
-- context sequence.
runInput :: FilePath -> ReactionFormat -> FilePath -> FilePath -> FilePath -> IO ()
runInput rsFile format ctxFile outputFile annotationFile = do
  (rs, ctx) <- readInput rsFile format ctxFile
  if ctx == []
    then error "ERROR: No context specified."
    else return ()

  let res = run rs ctx

  writeOutput rs (makeInteractiveProcess ctx res) format outputFile annotationFile

-- Runs an interactive simulation of the supplied reaction system.
interactiveRun :: FilePath -> ReactionFormat -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
interactiveRun rsFile format ctxFile outputFile annotationFile contextOutFile = do
  (rs, contexts) <- readInput rsFile format ctxFile

  let results = if contexts /= []
                then run rs contexts
                else [Set.empty]

  if contexts /= []
    then do
    putStrLn "Context sequence provided.  The description of the last reached state follows.\n"

    let state = (last contexts) `Set.union` (head results)
    TextIO.putStr $ annotateFunc rs (length contexts - 1) (last contexts) (head results)
    TextIO.putStrLn $ "New result: " `Text.append` (showSpaceSymbols state)
    putStrLn ""
    else do
    putStrLn "No context sequence provided, starting from scratch.\n"

  log <- go rs (last results) (length contexts) []
  let (furtherContexts, furtherResults) = unzip $ reverse log
      allContexts = contexts ++ furtherContexts
      allResults  = results  ++ furtherResults

  writeOutput rs (makeInteractiveProcess allContexts allResults) format outputFile annotationFile

  where go :: ReactionSystem -> Result -> Int -> [(Context, Result)] -> IO [(Context, Result)]
        go rs@(ReactionSystem _ reactions) res step acc = do
          putStr "Next context: "
          hFlush stdout
          ln <- TextIO.getLine
          putStrLn ""

          if ln == "STOP"
            then return acc
            else do
            let ctx = readSpaceSymbols ln
                state = ctx `Set.union` res
                newRes = apply reactions state

            TextIO.putStr $ annotateFunc rs step ctx res
            TextIO.putStrLn $ "New result: " `Text.append` (showSpaceSymbols newRes)
            putStrLn ""

            go rs newRes (step + 1) ((ctx, newRes):acc)

        annotateFunc = case format of
          Plain -> annotateStatePlain
          Arrow -> annotateStateArrow

reactionFormat = Arg.Type { Arg.parser = \val -> case val of
                               "plain" -> Right $ Plain
                               "arrow" -> Right $ Arrow
                               str -> Left $ "Unknown reaction format: " ++ show str
                          , Arg.name = "arrow|plain"
                          , Arg.defaultValue = Just Arrow
                          }

reactionFormatOpt = Arg.option ['f'] ["format"] reactionFormat Arrow
                    "\n    The format of the reaction description.\n\n\
\    The default value of this argument is \"arrow\", in which case the rules should be\n\
\    specified using a notation similar to the chemical notation:\n\n\
\        <left hand side> -> <right hand side> | <list of inhibitors>\n\n\
\    For example, a reaction which consumes a and b, produces c and d, and is inhibited\n\
\    by e and f, can be written as follows:\n\n\
\        a+b->c+d|e f\n\n\
\    If the value of this argument is \"plain\", the rules are specified as lists of\n\
\    reactants, inhibitors, and products respectively.  Thus, the same reaction can be\n\
\    written in the following way:\n\n\
\        a b, e f, c d\n"

contextFileOpt = Arg.option ['x'] ["context"] (Arg.optional "" Arg.file) ""
                 "\n    The file listing the contexts of an interactive process.\n\n\
\    If the context file is given, it should contain one context per line, each context\n\
\    being represented as a list of symbols.\n"

outputFileOpt = Arg.option ['o'] ["output"] (Arg.optional "" Arg.file) ""
                "\n    The file to write the output to.\n\n\
\    If no output file is specified, the output is written to the standard output.\n"

annotateOpt = Arg.option ['a'] ["annotate"] (Arg.optional "" Arg.file) ""
                "\n    The file to write annotated output to.\n\n\
\    If this option is specified, the simulator will write a detailed description of\n\
\    the activity of the system during the simulation.  For each step, it will write\n\
\    the context, the latest result, the current state, and the enabled rules.\n"

interactiveOpt = Arg.option ['i'] ["interact"] (Arg.optional False Arg.boolean) False
                 "\n    Start an interactive simulation session.\n\n\
\    If this option is specified, the simulator will explicitly ask the user for\n\
\    contexts and will print out the next state interactively.  If an output file is\n\
\    specified via --output, the whole result sequence will be written to the output\n\
\    file.  In a similar way, if an annotation file is specified, the annotated\n\
\    description of the interactive process will be produced."

contextOutOpt = Arg.option ['c'] ["output-context"] (Arg.optional "" Arg.file) ""
                "\n    The file to output the recorded context sequence to.\n\n\
\    If a file is specified, the complete sequence of contexts supplied to the reaction\n\
\    system during an interactive run will be written to it."

runCmd = Cmd.Command { Cmd.name = "run"
                     , Cmd.action = Cmd.withNonOption Arg.file $
                                    \rsFile ->
                                    Cmd.withOption reactionFormatOpt $
                                    \format ->
                                    Cmd.withOption contextFileOpt $
                                    \contextFile ->
                                    Cmd.withOption outputFileOpt $
                                    \outputFile ->
                                    Cmd.withOption annotateOpt $
                                    \annotationFile ->
                                    Cmd.io $ runInput rsFile format contextFile outputFile annotationFile
                     , Cmd.description = "Run the simulation of the reaction system given in FILE.\n\n\
\The input file should contain a description of the reaction system and, optionally, a\n\
\list of contexts to run the simulation in.  If the reaction system and the contexts\n\
\are given in the same file, they should be separated by a line containing three\n\
\dashes: \"---\".\n"
              }

interactCmd = Cmd.Command { Cmd.name = "interact"
                          , Cmd.action = Cmd.withNonOption Arg.file $
                                         \rsFile ->
                                         Cmd.withOption reactionFormatOpt $
                                         \format ->
                                         Cmd.withOption contextFileOpt $
                                         \contextFile ->
                                         Cmd.withOption outputFileOpt $
                                         \outputFile ->
                                         Cmd.withOption annotateOpt $
                                         \annotationFile ->
                                         Cmd.withOption contextOutOpt $
                                         \contextOutFile ->
                                         Cmd.io $ interactiveRun rsFile format contextFile outputFile annotationFile contextOutFile
                          , Cmd.description = "Start an interactive simulation session.\n\n\
\In this mode, the simulator will explicitly ask the user for contexts and will print\n\
\out the next state interactively.  If a context sequence is specified in the input file\n\
\or in a separate context file, the simulator will first run the reaction system with\n\
\the given context sequence and will start the interactive session at the last state.\n"
                          }

help = Cmd.Command { Cmd.name = "help"
                   , Cmd.action = Cmd.io $ showUsage brsimCommands
                   , Cmd.description = "Show this usage information."
               }

brsimCommand = Cmd.Command { Cmd.name = "brsim"
                           , Cmd.action = Cmd.io $ do
                             putStrLn "ERROR: No reaction system specified.  Showing usage information.\n"
                             showUsage brsimCommands
                           , Cmd.description = "A Basic Reaction System Simulator."
                           }

brsimCommands :: Cmd.Commands
brsimCommands = Cmd.Node brsimCommand [Cmd.Node runCmd [], Cmd.Node interactCmd [], Cmd.Node help []]

main :: IO ()
main = single brsimCommands
