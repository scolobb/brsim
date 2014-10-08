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
                  , reduce
                  , BehaviourGraph
                  , buildBehaviourGraph
                  , conservedInGraph
                  ) where

import ReactionSystems
import qualified Data.Set as Set
import Data.List (subsequences,partition)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS (components)
import qualified Data.Map as Map
import Data.Tuple (swap)

-- Since we use 'Set.toAscList', the empty set always comes first.
subsets :: Ord a =>  Set.Set a -> [Set.Set a]
subsets = map Set.fromAscList . subsequences . Set.toAscList

intersects :: Ord a =>  Set.Set a -> Set.Set a -> Bool
a `intersects` b = not $ Set.null $ a `Set.intersection` b

-- | Checks if the supplied set of species is conserved by the
-- reaction system.
conserved :: ReactionSystem -> Symbols -> Bool
conserved sys@(ReactionSystem _ rs) m =
  all (\sub -> let ressubs = apply rs sub
               in m `intersects` sub == m `intersects` ressubs
      ) $ subsets $ support sys

-- | Reduces the supplied reaction system to a set T: the new system
-- conserves the same sets with respect to the background set as the
-- original system does with respect to T.
--
-- For details, we refer to the soon-to-be-published paper "Dependency
-- Graphs and Mass Conservation in Reaction Systems".
reduce :: ReactionSystem -> Symbols -> ReactionSystem
reduce (ReactionSystem _ rs) t = makeReactionSystem' $ Set.map reduceReaction $ Set.filter isGood rs
  where isGood (Reaction r _ _) = r `Set.isSubsetOf` t
        reduceReaction (Reaction r i p) = Reaction r (i `Set.intersection` t) (p `Set.intersection` t)

type BehaviourGraph = Gr Symbols ()

-- | Builds the behaviour graph of a reaction system.
buildBehaviourGraph :: ReactionSystem -> BehaviourGraph
buildBehaviourGraph rs@(ReactionSystem s _) =
  let vs = zip [1..] $ subsets s
      vmap = Map.fromList $ map swap vs
      es = map (\(i, subs) -> (i, vmap Map.!  applyRS rs subs  , ())) vs
  in mkGraph vs es

-- Describes the relationship a set may be in with a set of sets: it
-- either intersects all of them, is disjoint from all of them, or
-- intersects some of them and is disjoint from some other of them.
data IntersectionKind = IntersectsAll | DisjointAll | Mixed
                      deriving (Show, Read, Eq, Ord)

-- Determines in which kind of intersection relation a set is with
-- respect to a given list of sets.
--
-- If the list of sets is empty, the behaviour of the function is
-- undefined.
interKind :: Ord a => [Set.Set a] -> Set.Set a -> IntersectionKind
interKind ss m = case partition (m `intersects`) ss of
  (_, []) -> IntersectsAll
  ([], _) -> DisjointAll
  _       -> Mixed

-- Determines in which kind of intersection relation a set of symbols
-- is with the set of vertices of a behaviour graph.
grInterKind :: BehaviourGraph -> [Node] -> Symbols -> IntersectionKind
grInterKind gr vs = interKind (map (lab gr) vs)

-- Checks if a set is consistent with the supplied vertices of the
-- behaviour graph.
isConsistent :: BehaviourGraph -> [Node] -> Symbols -> Bool
isConsistent gr vs = (`elem` [IntersectsAll, DisjointAll]) . grInterKind gr vs

-- | Given the behaviour graph of a reaction system, checks if the
-- supplied set is conserved.
conservedInGraph :: BehaviourGraph -> Symbols -> Bool
conservedInGraph gr m = all (isConsistent' gr m) (components gr)
  where isConsistent' g = flip (isConsistent g)
