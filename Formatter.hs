{-# LANGUAGE OverloadedStrings #-}
{- Formatter.hs

Pretty-prints the components of reaction systems to string.-}

module Formatter ( showSymbol
                 , showSymbols
                 , showListOfListsOfSymbols
                 ) where

import ReactionSystems
import qualified Data.Text.Lazy as Text
import qualified Data.Set as Set

-- | Pretty-print the supplied symbol.
showSymbol :: Symbol -> Text.Text
showSymbol = Text.pack . name

-- | Pretty-print the supplied set of symbol as a list.
showSymbols :: Symbols -> Text.Text
showSymbols = Text.intercalate " " . Set.toList . Set.map showSymbol

-- | Pretty-print a list of lists of symbols.
showListOfListsOfSymbols :: [Symbols] -> Text.Text
showListOfListsOfSymbols = Text.unlines . map showSymbols
