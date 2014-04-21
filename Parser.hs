{-# LANGUAGE OverloadedStrings #-}
{- Parser.hs

Reads the components of a reaction system from string.-}

module Parser ( readSymbols
              , readPlusSymbols
              , readSpaceSymbols
              , readPlainReaction
              ) where

import ReactionSystems
import qualified Data.Text as Text
import qualified Data.Set as Set

-- Strips leading and trailing whitespace from all texts in the
-- supplied list and discards the empty elements afterwards.
stripFilter :: [Text.Text] -> [Text.Text]
stripFilter = filter (not . Text.null) . map Text.strip

-- | Parses a list of symbols (second argument) separated by the given
-- sequence of symbols (first argument).
readSymbols :: Text.Text -> Text.Text -> Symbols
readSymbols sep = Set.fromList . map (Symbol . Text.unpack) . stripFilter . Text.splitOn sep

-- | Parses a plus-separated list of symbols.
readPlusSymbols = readSymbols "+"

-- | Parses a space-separated list of symbols.
readSpaceSymbols = readSymbols " "

-- | Parses a reaction in plain format.  A reaction which consumes
-- symbols 'a' and 'b', produces 'c' and 'd', and is inhibited by 'e'
-- and 'f' is written as follows: a b, e f, c d.
readPlainReaction :: Text.Text -> Reaction
readPlainReaction txt = let parts = Text.splitOn "," txt
                            [rcts, inh, prods] = map readSpaceSymbols parts
                        in Reaction rcts inh prods
