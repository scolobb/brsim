{-# LANGUAGE OverloadedStrings #-}
{- brsim.hs

A Basic Reaction Systems Simulator. -}

import ReactionSystems
import Parser
import qualified System.Console.Argument as Arg
import qualified System.Console.Command as Cmd
import System.Console.Program
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as TextIO

-- | The possible reaction description formats.
data ReactionFormat = Plain -- ^ A reaction is given as three lists of symbol names.
                    | Arrow -- ^ A reaction is given in a notation similar to the chemical one.
                    deriving (Eq, Ord, Show, Read)

-- | Reads the supplied file containing the description of the
-- reaction system and, maybe, the list of contexts.  If a separate
-- context file is specified, the list of context in the reaction
-- system file is ignored.
readInput :: FilePath -> ReactionFormat -> FilePath -> IO (ReactionSystem, [Context])
readInput rsFile format ctxFile = do
  desc <- TextIO.readFile rsFile
  let (txtRs, maybeTxtCtx) = Text.breakOn "\n---" desc

  txtCtx <- if ctxFile /= ""
            then TextIO.readFile ctxFile
            else if Text.null maybeTxtCtx
                 then error "ERROR: No context specified."
                 else return $ Text.drop 4 maybeTxtCtx

  let rules = case format of
        Plain -> readPlainReactions txtRs
        Arrow -> readArrowReactions txtRs

      contexts = readListOfListsOfSymbols txtCtx

  return (makeReactionSystem rules, contexts)

-- | Runs the simulation of the supplied reaction system with the
-- given context sequence.
runInput :: FilePath -> ReactionFormat -> FilePath -> FilePath -> IO ()
runInput rsFile format ctxFile outputFile = do
  (rs, ctx) <- readInput rsFile format ctxFile

  let res = run rs ctx

  outputFunc $ show res

  where outputFunc = case outputFile of
          "" -> putStr
          file -> writeFile file

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
                "\n    The file to write the output to.  If no output file is specified,\n\
\    the output is written to the standard output."

runCmd = Cmd.Command { Cmd.name = "run"
                     , Cmd.action = Cmd.withNonOption Arg.file $
                                    \rsFile ->
                                    Cmd.withOption reactionFormatOpt $
                                    \format ->
                                    Cmd.withOption contextFileOpt $
                                    \contextFile ->
                                    Cmd.withOption outputFileOpt $
                                    \outputFile ->
                                    Cmd.io $ runInput rsFile format contextFile outputFile
                     , Cmd.description = "Run the simulation of the reaction system given in FILE.\n\n\
\The input file should contain a description of the reaction system and, optionally, a\n\
\list of contexts to run the simulation in.  If the reaction system and the contexts\n\
\are given in the same file, they should be separated by a line containing three\n\
\dashes: \"---\".\n"
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
brsimCommands = Cmd.Node brsimCommand [Cmd.Node runCmd [], Cmd.Node help []]

main :: IO ()
main = single brsimCommands
