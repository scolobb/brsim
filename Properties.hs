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
                  ) where

import ReactionSystems
import qualified Data.Set as Set
import Data.List (subsequences,partition,(\\))
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS (components,scc,reachable)
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

-- Builds a subgraph of the given graph which only includes the
-- supplied nodes.
subgraph :: DynGraph gr => [Node] -> gr a b -> gr a b
subgraph vs =
  let base = Set.fromList vs
  in buildGr . ufold (\(inadj, v, l, outadj) res ->
                       if v `Set.notMember` base
                       then res
                       else let good = (`Set.member` base) . snd
                                outadj' = filter good outadj
                                inadj'  = filter good inadj
                            in (inadj', v, l, outadj'):res
                     ) []

-- Checks if there is a directed edge between two vertices.
isEdge :: Graph gr => gr a b -> Node -> Node -> Bool
isEdge gr v w = w `elem` (suc gr v)

-- Computes the condensation of the given graph, i.e., the graph of
-- its strongly connected components.
condensation :: Graph gr => gr a b -> gr [Node] ()
condensation gr = let sccs = scc gr
                      vs = zip [1..] sccs
                      vMap = Map.fromList $ map swap vs
                      es = do
                        c1 <- sccs
                        c2 <- sccs

                        if (c1 /= c2) && ( or [ isEdge gr v w | v <- c1, w <- c2 ] )
                          then [(vMap Map.! c1, vMap Map.! c2, ())]
                          else []
                  in mkGraph vs es

-- Computes the source sets of the given graph.  The behaviour of this
-- function is undefined when the graph is not a DAG.
--
-- This is an implementation of Algorithm 3.1 of our paper.
sourceSetsDAG :: DynGraph gr => gr a b -> [[Node]]
sourceSetsDAG gr | isEmpty gr = [[]]
                 | otherwise =
  let vs = nodes gr
      (s:_) = sources gr
      t = reachable s gr -- The descendants of 's'.

      gminus = subgraph (vs \\ t  ) gr
      gplus  = subgraph (vs \\ [s]) gr

      gminusSrc = sourceSetsDAG gminus
      gplusSrc  = sourceSetsDAG gplus

  in gminusSrc ++ [ s:src | src <- gplusSrc ]
