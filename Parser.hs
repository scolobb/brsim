{-# LANGUAGE OverloadedStrings #-}
{- Parser.hs

Reads the components of a reaction system from string.

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

-- Removes symbols with dots as their names.  Such symbols stand for
-- the empty set.
dropEmpty :: [Symbol] -> [Symbol]
dropEmpty = filter ((/=) "." . name)

-- | Parses a list of symbols (second argument) separated by the given
-- sequence of symbols (first argument).
readSymbols :: Text.Text -> Text.Text -> Symbols
readSymbols sep = Set.fromList . dropEmpty . map (Symbol . Text.unpack) . stripFilter . Text.splitOn sep

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
