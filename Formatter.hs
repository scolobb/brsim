{-# LANGUAGE OverloadedStrings #-}
{- Formatter.hs

Pretty-prints the components of reaction systems to string.

Copyright 2014 by Sergiu Ivanov <sergiu.ivanov@u-pec.fr>

This file is part of brsim.

brsim is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your
option) any later version.

brsim is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

-}

module Formatter ( showSymbol
                 , showSpaceSymbols
                 , showPlusSymbols
                 , showSetSymbols
                 , showListOfListsOfSymbols
                 , showPlainReaction
                 , showArrowReaction
                 , annotateStatePlain
                 , annotateStateArrow
                 , annotatePlain
                 , annotateArrow
                 , showSymbolGraph
                 , showSymbolsGraph
                 , graph2dot
                 ) where

import ReactionSystems
import qualified Data.Text.Lazy as Text
import qualified Data.Set as Set
import Data.Graph.Inductive.Graph hiding (Context)
import Data.Graph.Inductive.PatriciaTree
import Data.Maybe (fromJust)
import Text.Dot as Dot

-- | Pretty-print the supplied symbol.
showSymbol :: Symbol -> Text.Text
showSymbol = Text.pack . name

-- Replaces empty text with a full-stop.
handleEmpty :: Text.Text -> Text.Text
handleEmpty t | Text.null t = "."
handleEmpty t | otherwise = t

-- Pretty-print the supplied set of symbol as a list with the given
-- separator.
showSymbols :: Bool -> Text.Text -> Symbols -> Text.Text
showSymbols mustHandle sep = (if mustHandle then handleEmpty else id)
                             . Text.intercalate sep . Set.toList . Set.map showSymbol

-- | Pretty-print the supplied set of symbol as a space-separated
-- list.
showSpaceSymbols = showSymbols True " "

-- | Pretty-print the supplied set of symbol as a plus-separated
-- list.
showPlusSymbols = showSymbols True "+"

surround :: Text.Text -> Text.Text -> Text.Text -> Text.Text
surround pref suff xs = pref `Text.append` xs `Text.append` suff

-- | Pretty-print the supplied set of symbols in the usual set
-- notation (e.g. /{x,y}/).
showSetSymbols = surround "{" "}" . showSymbols False ","

-- | Pretty-print a list of lists of symbols.
showListOfListsOfSymbols :: [Symbols] -> Text.Text
showListOfListsOfSymbols = Text.unlines . map showSpaceSymbols

-- | Pretty-print a reaction in plain format.
showPlainReaction :: Reaction -> Text.Text
showPlainReaction (Reaction rcts inh prod) = Text.intercalate ", " $ map showSpaceSymbols [rcts, inh, prod]

-- | Pretty-print a reaction in arrow format.
showArrowReaction :: Reaction -> Text.Text
showArrowReaction (Reaction rcts inh prod) =
  let txtRcts = showPlusSymbols  rcts
      txtProd = showPlusSymbols  prod
      txtInh  = showSpaceSymbols inh
  in txtRcts `Text.append` "->" `Text.append` txtProd `Text.append`
     if not $ Set.null inh then "|" `Text.append` txtInh else ""

-- Annotates a state of the supplied reaction system.
annotateState :: (Reaction -> Text.Text) -> ReactionSystem -> Int -> Context -> Result -> Text.Text
annotateState rformat (ReactionSystem _ rs) step ctx res =
  let state = (ctx `Set.union` res)
  in Text.unlines $ [ "STEP " `Text.append` (Text.pack $ show step)
     , "Context:       " `Text.append` showSpaceSymbols ctx
     , "Last result:   " `Text.append` showSpaceSymbols res
     , "State:         " `Text.append` showSpaceSymbols state
     , "Enabled reactions:"
     ] ++ (map (Text.append "  " . rformat) $ filter ((flip enabled) state) $ Set.toList rs)

-- | Annotates a state of the supplied reaction system printing the
-- reactions in plain format.
annotateStatePlain :: ReactionSystem -> Int -> Context -> Result -> Text.Text
annotateStatePlain = annotateState showPlainReaction

-- | Annotates a state of the supplied reaction system printing the
-- reactions in arrow format.
annotateStateArrow :: ReactionSystem -> Int -> Context -> Result -> Text.Text
annotateStateArrow = annotateState showArrowReaction

-- Annotate an interactive process of the supplied reaction system
-- using a supplied reaction formatting function.
annotate :: (Reaction -> Text.Text) -> ReactionSystem -> InteractiveProcess -> Text.Text
annotate rformat rs (InteractiveProcess contexts results) =
  Text.intercalate "\n" $ map annotateState' $ zip3 [0..] contexts results
  where annotateState' (n, ctx, res) = annotateState rformat rs n ctx res

-- | Annotate an interactive process of the supplied reaction system,
-- printing reactions in plain format.
annotatePlain :: ReactionSystem -> InteractiveProcess -> Text.Text
annotatePlain = annotate showPlainReaction

-- | Annotate an interactive process of the supplied reaction system,
-- printing reactions in arrow format.
annotateArrow :: ReactionSystem -> InteractiveProcess -> Text.Text
annotateArrow = annotate showArrowReaction

-- | Pretty prints a graph.
showGraph :: (a -> Text.Text) -> Gr a () -> Text.Text
showGraph showFunc gr =
  let labj = fromJust . lab gr
  in Text.unlines $ concatMap
     (\v -> let sc = suc gr v
                s = showFunc $ labj v
            in [ s `Text.append` " -> "
                 `Text.append` ( Text.intercalate " " $ map (showFunc . labj) sc )]
     ) $ nodes gr

-- | Pretty prints a graph of symbols.
showSymbolGraph :: Gr Symbol () -> Text.Text
showSymbolGraph = showGraph showSymbol

-- | Pretty prints a graph of sets of symbols (e.g. the behaviour
-- graph of a reaction system).
showSymbolsGraph :: Gr Symbols () -> Text.Text
showSymbolsGraph = showGraph showSetSymbols

-- | Produces the DOT format description of the given graph.
graph2dot' :: Graph gr => (a -> String) -> (b -> String) -> gr a b -> Dot.Dot ()
graph2dot' showNode showEdge gr = do
  mapM_ genNode $ labNodes gr
  mapM_ genEdge $ labEdges gr

  where genNode (v,vLab) = userNode (userNodeId v) [("label", showNode vLab)]
        genEdge (v,w,lab) = edge (userNodeId v) (userNodeId w)
                            [("label", showEdge lab)]

-- | Produce the DOT format description of the given graph in a string.
graph2dot :: Graph gr => (a -> String) -> (b -> String) -> gr a b -> String
graph2dot showNode showEdge = Dot.showDot . graph2dot' showNode showEdge
