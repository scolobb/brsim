{- Properties.hs

Defines tools for verifying some properties of reaction systems.

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

module Properties ( conserved
                  , buildBehaviourGraph
                  , listConservedSets
                  ) where

import ReactionSystems
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Array as Array
import Data.Graph
import Data.List (subsequences)
import Data.Tuple (swap)

-- Since we use 'Set.toAscList', the empty set always comes first.
subsets :: Ord a =>  Set.Set a -> [Set.Set a]
subsets = map Set.fromAscList . subsequences . Set.toAscList

intersects :: Ord a =>  Set.Set a -> Set.Set a -> Bool
a `intersects` b = not $ Set.null $ a `Set.intersection` b

conserved :: ReactionSystem -> Symbols -> Bool
conserved sys@(ReactionSystem _ rs) m =
  all (\sub -> let ressubs = apply rs sub
               in m `intersects` sub == m `intersects` ressubs
      ) $ subsets $ support sys

type SubsetArray = Array.Array Int Symbols
type SubsetMap = Map.Map Symbols Int

data BehaviourGraph = BehaviourGraph { behaviourGraph :: Graph
                                     , subsetArray    :: SubsetArray
                                     , subsetMap      :: SubsetMap
                                     } deriving (Show, Read, Eq)

buildSubsetArray :: Symbols -> SubsetArray
buildSubsetArray ss = Array.listArray (1, 2^(Set.size ss)) $ subsets ss

buildSubsetMap :: SubsetArray -> SubsetMap
buildSubsetMap = Map.fromList . map swap . Array.assocs

-- | Builds the behaviour graph of a reaction system.  The subsets for
-- the subset array and subset map are taken from the _support_ of the
-- reaction system.
buildBehaviourGraph :: ReactionSystem -> BehaviourGraph
buildBehaviourGraph rs =
  let sarr = buildSubsetArray $ support rs
      smap = buildSubsetMap sarr
      -- By definition, we don't get out of the
      -- support set when we apply some reactions.
      edges = map (\(i, subs) -> (i, smap Map.! (applyRS rs subs) )) $ Array.assocs sarr
      gr = buildG (Array.bounds sarr) edges
  in BehaviourGraph gr sarr smap

listConservedSets :: ReactionSystem -> [Symbols]
listConservedSets rs@(ReactionSystem u _) = filter (conserved rs) $ tail $ subsets u
