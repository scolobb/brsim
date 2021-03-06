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
                  , ConsDepGraph
                  , buildConsDepGraph'
                  , buildConsDepGraph
                  , listConservedSets
                  ) where

import ReactionSystems
import qualified Data.Set as Set
import Data.List
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.NodeMap
import qualified Data.Map as Map
import Data.Tuple (swap)
import Data.Maybe (fromJust)

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

labj gr = fromJust . lab gr
mlabj gr = map (labj gr)

-- Determines in which kind of intersection relation a set of symbols
-- is with the set of vertices of a behaviour graph.
grInterKind :: BehaviourGraph -> [Node] -> Symbols -> IntersectionKind
grInterKind gr vs = interKind (mlabj gr vs)

-- Checks if a set is consistent with the supplied vertices of the
-- behaviour graph.
isConsistent :: BehaviourGraph -> [Node] -> Symbols -> Bool
isConsistent gr vs = (`elem` [IntersectsAll, DisjointAll]) . grInterKind gr vs

-- | Given the behaviour graph of a reaction system, checks if the
-- supplied set is conserved.
conservedInGraph :: BehaviourGraph -> Symbols -> Bool
conservedInGraph gr m = all (isConsistent' gr m) (components gr)
  where isConsistent' g = flip (isConsistent g)

-- Computes the cover (the union of the associated sets) of a set of
-- vertices of the behaviour graph.
cover :: BehaviourGraph -> [Node] -> Symbols
cover gr vs = Set.unions $ mlabj gr vs

-- Lists the elements appearing in singletons in the given set of
-- vertices of the behaviour graph.
singletons :: BehaviourGraph -> [Node] -> [Symbol]
singletons gr vs = concatMap Set.toList [ v | v <- mlabj gr vs, Set.size v == 1 ]

type ConsDepGraph = Gr Symbol ()

-- Builds the conservation dependency graph starting from the
-- behaviour graph and the set of symbols to include.
buildConsDepGraph' :: BehaviourGraph -> Symbols -> ConsDepGraph
buildConsDepGraph' gr ss =
  let vs = zip [1..] $ Set.elems ss
      es' = concatMap (\cmp ->
                        let snglts = singletons gr cmp
                            cvr = Set.elems $ cover gr cmp
                        in [ (x, y) | x <- snglts, y <- cvr, x /= y ]
                      ) $ components gr
      smap = Map.fromList $ map swap vs
      es = map (\(x, y) -> (smap Map.! x, smap Map.! y, ())) es'
  in mkGraph vs es

buildConsDepGraph :: ReactionSystem -> ConsDepGraph
buildConsDepGraph rs@(ReactionSystem s _) = buildConsDepGraph' (buildBehaviourGraph rs) s

-- Lists the source vertices of the supplied directed graph.
sources :: Graph gr => gr a b -> [Node]
sources gr = [ v | v <- nodes gr , indeg gr v == 0 ]

descendants :: Graph gr => Node -> gr a b -> [Node]
descendants = reachable

ancestors :: Graph gr => Node -> gr a b -> [Node]
ancestors v = rdfs [v]

-- Computes the source sets of the given graph.  The behaviour of this
-- function is undefined when the graph is not a DAG.
--
-- This is an implementation of Algorithm 3.1 of our paper.
sourceSetsDAG :: DynGraph gr => gr a b -> [[Node]]
sourceSetsDAG gr | isEmpty gr = [[]]
                 | otherwise =
  let vs = nodes gr
      (s:_) = sources gr
      t = descendants s gr

      gminus = subgraph (vs \\ t  ) gr
      gplus  = subgraph (vs \\ [s]) gr

      gminusSrc = sourceSetsDAG gminus
      gplusSrc  = sourceSetsDAG gplus

  in gminusSrc ++ [ s:src | src <- gplusSrc ]

listConservedSets :: ReactionSystem -> [Symbols]
listConservedSets rs@(ReactionSystem s _) =
  -- 1. Compute the behaviour graph.
  let bhg = buildBehaviourGraph rs

      -- 2. Compute the connected components of 'bhg' and analyse
      -- them.
      cmps = components bhg

      -- The cover of the component containing the empty set.  This
      -- component always exists and is unique.
      p = Set.toList $ Set.unions $ fromJust $ find (Set.empty `elem`) $ map (mlabj bhg) cmps

      -- The elements appearing as singletons in the components whose
      -- cover is the full set.
      q  = nub $ concatMap (singletons bhg) $ filter ((== s) . cover bhg) cmps
      q' = Set.fromList q

      -- 3. Compute the conservation dependency graph.
      cdg = buildConsDepGraph' bhg s
      cdgMap = fromGraph cdg

      -- 4. Compute the condensation of 'cdg'.
      cdgc = condensation cdg

      -- Extend the set 'p' with its descendants in the conservation
      -- dependency graph, and 'q' with its ancestors.
      p_desc = concatMap ( ((flip descendants) cdg) . fst) $ mkNodes_ cdgMap p
      q_anc  = concatMap ( ((flip ancestors  ) cdg) . fst) $ mkNodes_ cdgMap q

      -- 5. Compute the reduced condensation of the conservation
      -- dependency graph by removing from it all the nodes which
      -- contain elements from 'p_desc' or 'q_anc'.
      good = null . (Data.List.intersect $ p_desc ++ q_anc)
      cdgc' = labnfilter (good . snd) cdgc

      -- 6. Compute the source sets of 'cdgc''.
      ssets = sourceSetsDAG cdgc'

      -- 7. Add 'q' to each source set and test whether it is
      -- conserved.

      -- Gets the symbols included in the given set of vertices of the
      -- condensation of 'cdg'.
      symbs = Set.fromList . mlabj cdg . concat . mlabj cdgc'

      ms = map (Set.union q' . symbs) ssets
  in (if Set.null q' then [] else [Set.empty]) ++ filter (conservedInGraph bhg) ms
