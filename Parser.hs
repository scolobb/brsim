{-# LANGUAGE OverloadedStrings #-}
{- Parser.hs

Reads the components of a reaction system from string.-}

module Parser ( readSymbols
              , readPlusSymbols
              , readSpaceSymbols
              , readPlainReaction
              , readArrowReaction
              , readPlainReactions
              , readArrowReactions
              , readListOfListsOfSymbols
              ) where

import ReactionSystems
import qualified Data.Text.Lazy as Text
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

-- | Parses a reaction in arrow format.  A reaction which consumes
-- symbols 'a' and 'b', produces 'c' and 'd', and is inhibited by 'e'
-- and 'f' is written as follows: a+b -> c+d|e f.
readArrowReaction :: Text.Text -> Reaction
readArrowReaction txt =
  let (txtRcts, rest) = Text.breakOn "->" txt
      (txtProds, txtInh) = Text.breakOn "|" $ Text.drop 2 rest
      rcts  = readPlusSymbols  txtRcts
      prods = readPlusSymbols  txtProds
      inh   = if not $ Text.null txtInh
              then readSpaceSymbols $ Text.tail txtInh
              else Set.empty
  in Reaction rcts inh prods

-- Splits the supplied text (third argument) into lines, throws away
-- the lines for which the filtering function (first argument) is
-- true, and applies the processing function (second argument) to the
-- remaining lines.
--
-- This function will also discard whitespace lines.
mapGoodLines :: (Text.Text -> Bool) -> (Text.Text -> a) -> Text.Text -> [a]
mapGoodLines flt func = map func . filter (not . flt) . stripFilter . Text.lines

commentLine = Text.isPrefixOf "#"

-- | Reads a list of reactions in plain format.  Reactions are given
-- one per line.  Whitespace lines and lines starting with the hash
-- symbol are discarded.
readPlainReactions :: Text.Text -> [Reaction]
readPlainReactions = mapGoodLines commentLine readPlainReaction

-- | Reads a list of reactions in arrow format.  Reactions are given
-- one per line.  Whitespace lines and lines starting with the hash
-- symbol are discarded.
readArrowReactions :: Text.Text -> [Reaction]
readArrowReactions = mapGoodLines commentLine readArrowReaction

-- | Reads a list of lists of symbol.  Lists of symbols are given one
-- per line.  Whitespace lines and lines starting with the hash symbol
-- are discarded.
readListOfListsOfSymbols :: Text.Text -> [Symbols]
readListOfListsOfSymbols = mapGoodLines commentLine readSpaceSymbols
