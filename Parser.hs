{-# LANGUAGE OverloadedStrings #-}
{- Parser.hs

Reads the components of a reaction system from string.-}

module Parser ( readSymbols
              , readPlusSymbols
              , readSpaceSymbols
              ) where

import ReactionSystems
import qualified Data.Text as Text
import qualified Data.Set as Set

-- | Parses a list of symbols (second argument) separated by the given
-- sequence of symbols (first argument).
readSymbols :: Text.Text -> Text.Text -> Symbols
readSymbols sep = Set.fromList . map (Symbol . Text.unpack) . filter (not . Text.null)
                  . map Text.strip . Text.splitOn sep

-- | Parses a plus-separated list of symbols.
readPlusSymbols = readSymbols "+"

-- | Parses a space-separated list of symbols.
readSpaceSymbols = readSymbols " "
