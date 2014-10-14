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
                 , showListOfListsOfSymbols
                 , showPlainReaction
                 , showArrowReaction
                 , annotateStatePlain
                 , annotateStateArrow
                 , annotatePlain
                 , annotateArrow
                 , showSymbolGraph
                 ) where

import ReactionSystems
import qualified Data.Text.Lazy as Text
import qualified Data.Set as Set
import Data.List (intercalate)
import Data.Graph.Inductive.Graph (nodes,suc,lab)
import Data.Graph.Inductive.PatriciaTree
import Data.Maybe (fromJust)

-- | Pretty-print the supplied symbol.
showSymbol :: Symbol -> Text.Text
showSymbol = Text.pack . name

-- Replaces empty text with a full-stop.
handleEmpty :: Text.Text -> Text.Text
handleEmpty t | Text.null t = "."
handleEmpty t | otherwise = t

-- Pretty-print the supplied set of symbol as a list with the given
-- separator.
showSymbols :: Text.Text -> Symbols -> Text.Text
showSymbols sep = handleEmpty . Text.intercalate sep . Set.toList . Set.map showSymbol

-- | Pretty-print the supplied set of symbol as a space-separated
-- list.
showSpaceSymbols = showSymbols " "

-- | Pretty-print the supplied set of symbol as a plus-separated
-- list.
showPlusSymbols = showSymbols "+"

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

-- | Pretty prints a graph of symbols.
showSymbolGraph :: Gr Symbol () -> Text.Text
showSymbolGraph gr = let labj = fromJust . lab gr
                     in Text.unlines
                        $ concatMap (\v -> let sc = suc gr v
                                               s = showSymbol $ labj v
                                           in [ s `Text.append` " -> " `Text.append`
                                                if null sc then ""
                                                else showSpaceSymbols (Set.fromList $ map labj sc) ]
                                    ) $ nodes gr
