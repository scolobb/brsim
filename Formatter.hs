{-# LANGUAGE OverloadedStrings #-}
{- Formatter.hs

Pretty-prints the components of reaction systems to string.-}

module Formatter ( showSymbol
                 , showSpaceSymbols
                 , showPlusSymbols
                 , showListOfListsOfSymbols
                 , showPlainReaction
                 , showArrowReaction
                 , annotatePlain
                 , annotateArrow
                 ) where

import ReactionSystems
import qualified Data.Text.Lazy as Text
import qualified Data.Set as Set
import Data.List (intercalate)

-- | Pretty-print the supplied symbol.
showSymbol :: Symbol -> Text.Text
showSymbol = Text.pack . name

-- Pretty-print the supplied set of symbol as a list with the given
-- separator.
showSymbols :: Text.Text -> Symbols -> Text.Text
showSymbols _ ss | Set.null ss = "<empty>"
showSymbols sep ss | otherwise = (Text.intercalate sep . Set.toList . Set.map showSymbol) ss

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
  in txtRcts `Text.append` "->" `Text.append` txtProd `Text.append` "|" `Text.append` txtInh

-- Annotate an interactive process of the supplied reaction system
-- using a supplied reaction formatting function.
annotate :: (Reaction -> Text.Text) -> ReactionSystem -> InteractiveProcess -> Text.Text
annotate rformat (ReactionSystem _ rs) (InteractiveProcess contexts results) =
  Text.unlines $ intercalate [""] $ map annotateState $ zip3 [1..] contexts results
  where annotateState (n, ctx, res) =
          let state = (ctx `Set.union` res)
          in [ "STEP " `Text.append` (Text.pack $ show n)
             , "Context:     " `Text.append` showSpaceSymbols ctx
             , "Last result: " `Text.append` showSpaceSymbols res
             , "State:       " `Text.append` showSpaceSymbols state
             , "Enabled rules:"
             ] ++ (map (Text.append "  " . rformat) $ filter ((flip enabled) state) $ Set.toList rs)

-- | Annotate an interactive process of the supplied reaction system,
-- printing rules in plain format.
annotatePlain :: ReactionSystem -> InteractiveProcess -> Text.Text
annotatePlain = annotate showPlainReaction

-- | Annotate an interactive process of the supplied reaction system,
-- printing rules in arrow format.
annotateArrow :: ReactionSystem -> InteractiveProcess -> Text.Text
annotateArrow = annotate showArrowReaction
