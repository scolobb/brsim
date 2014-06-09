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
                  , LabelledGraph(..)
                  , BehaviourGraph
                  , buildBehaviourGraph
                  , listConservedSets
                  ) where

import ReactionSystems
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Array as Array
import Data.Graph
import Data.Tree
import Data.List
import Data.Tuple (swap)
import qualified Data.IntMap as IntMap

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

data LabelledGraph a = LabelledGraph Graph (Array.Array Vertex a) (Map.Map a Vertex)

type BehaviourGraph = LabelledGraph Symbols

type SubsetArray = Array.Array Int Symbols
type SubsetMap = Map.Map Symbols Int

buildSubsetArray :: Symbols -> SubsetArray
buildSubsetArray ss = Array.listArray (1, 2^Set.size ss ) $ subsets ss

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
      edges = map (\(i, subs) -> (i, smap Map.!  applyRS rs subs  )) $ Array.assocs sarr
      gr = buildG (Array.bounds sarr) edges
  in LabelledGraph gr sarr smap

flattenedComponents :: Graph -> [[Vertex]]
flattenedComponents = map flatten . components

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
intersectionKind :: Ord a => [Set.Set a] -> Set.Set a -> IntersectionKind
intersectionKind ss m = case partition (m `intersects`) ss of
  (_, []) -> IntersectsAll
  ([], _) -> DisjointAll
  _       -> Mixed

-- Determines in which kind of intersection relation a set of symbols
-- is with the given connected component of a behaviour graph.
componentIntersectionKind :: BehaviourGraph -> [Vertex] -> Symbols -> IntersectionKind
componentIntersectionKind (LabelledGraph _ sarr _) cmp = intersectionKind (map (sarr Array.!) cmp)

-- Checks if a set is conserved in the given components of the
-- behaviour graph.
conservedInGraph :: BehaviourGraph -> [[Vertex]] -> Symbols -> Bool
conservedInGraph gr cmps m = all (`elem` [IntersectsAll, DisjointAll])
                             $ map (componentIntersectionKind' m gr)
                             cmps
  where componentIntersectionKind' ss g cmp = componentIntersectionKind g cmp ss

-- Finds all singleton sets which are associated with a vertex in a
-- given list of them.  Then puts all those sets together.
singletons :: BehaviourGraph -> [Vertex] -> Symbols
singletons (LabelledGraph _ sarr _) =
  Set.unions . map (\v -> let ss = sarr Array.! v
                          in if Set.size ss == 1 then ss else Set.empty)

-- Lists the source vertices of a given (directed) graph.
sources :: Graph -> [Vertex]
sources gr = [ v | (v, deg) <- Array.assocs $ indegree gr, deg == 0 ]

-- Builds the subgraph induced by the given vertices and also returns
-- a mapping of the indices of the vertex numbers of the subgraph to
-- the vertex numbers of the original graph.
subgraph :: Graph -> [Vertex] -> (Graph, IntMap.IntMap Vertex)
subgraph gr vs =
  let vsMap = IntMap.fromList $ zip vs [1..]

      remapEdge (v, w) = do
        v' <- IntMap.lookup v vsMap
        w' <- IntMap.lookup w vsMap
        return (v', w')

      newEdges = do
        e <- edges gr
        case remapEdge e of
          Just (v, w) -> [(v, w)]
          Nothing     -> []

      newGr = buildG (1, IntMap.size vsMap) newEdges
      resMap = IntMap.fromList $ map swap $ IntMap.assocs vsMap

  in (newGr, resMap)

-- Lists the descendants of a vertex in the graph.  Vertex 'w' is a
-- descendant of 'v' if 'w' is reachable from 'v'.
descendants :: Graph -> Vertex -> [Vertex]
descendants gr v = flatten $ head $ dfs gr [v]

-- Lists the ancestors of a vertex in the graph.  Vertex 'w' is an
-- ancestor of 'v' if 'v' is a descendant of 'w'.
ancestors :: Graph -> Vertex -> [Vertex]
ancestors gr = descendants (transposeG gr)

-- Computes the source sets of the given DAG.  The behaviour of this
-- function is undefined when the graph is not a DAG.
sourceSetsDAG :: Graph -> [[Vertex]]
sourceSetsDAG gr = case vertices gr of
  []  -> [[]]
  [v] -> [[], [v]]
  vs  -> let s = head $ sources gr
             t = descendants gr s

             (gplus,  plusMap ) = subgraph gr $ vs \\ [s]
             (gminus, minusMap) = subgraph gr $ vs \\ t

             gplusSrc  = map (map (plusMap  IntMap.!)) $ sourceSetsDAG gplus
             gminusSrc = map (map (minusMap IntMap.!)) $ sourceSetsDAG gminus
         in [[]] ++ gminusSrc ++ [ s:src | src <- gplusSrc ]

-- Taken from http://hackage.haskell.org/package/fgl-5.5.0.1/docs/Data-Graph-Inductive-Query-Monad.html .
infixr 8 ><
(><) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
(f >< g) (x,y) = (f x,g y)

-- For the given graph, computes the DAG of its strongly connected
-- components and returns the mapping of the numbers of vertices in
-- the new graph to the strongly connected components.
sccDAG :: Graph -> (Graph, IntMap.IntMap [Vertex])
sccDAG gr =
  let sccs = map flatten $ scc gr
      sccsMap = zip [1..] sccs
      resMap = IntMap.fromList sccsMap

      -- Map vertices to the indices of the connected components that
      -- contain them.
      vMap = IntMap.fromList $ do
        (idx, scc) <- sccsMap
        zip scc $ repeat idx

      dagEdges = nub $ map ( (vMap IntMap.!) >< (vMap IntMap.!) ) $ edges gr
      dag = buildG (1, IntMap.size resMap) dagEdges
  in (dag, resMap)

sets :: BehaviourGraph -> [Vertex] -> [Symbols]
sets (LabelledGraph _ sarr _) = map (sarr Array.!)

-- The conservation dependency graph.
type ConsDepGraph = LabelledGraph Symbol

-- Builds the conservation dependency graph based on the supplied
-- behaviour graph and the support set of the original reaction
-- system.
buildConsDepGraph :: BehaviourGraph -> Symbols -> ConsDepGraph
buildConsDepGraph bhg@(LabelledGraph gr _ _) supp =
  let cmps = flattenedComponents gr
      symbIdx = zip [1..] $ Set.toList supp
      symbArr = Array.array (1, Set.size supp) symbIdx
      symbMap = Map.fromList $ map swap symbIdx

      resEdges = [ (symbMap Map.! x, symbMap Map.! y)
                 | cmp <- cmps
                 , x <- Set.elems $ singletons bhg cmp
                 , y <- Set.elems $ Set.unions $ sets bhg cmp
                 , x /= y ]

      consDepGr = buildG (1, Set.size supp) resEdges
  in LabelledGraph consDepGr symbArr symbMap

listConservedSets :: ReactionSystem -> [Symbols]
listConservedSets rs =
  let bhg@(LabelledGraph gr _ _) = buildBehaviourGraph rs
      cmps = flattenedComponents gr
      supp = support rs

      -- There always is a component with an empty set.
      (Just emptyCmp) = find (elem Set.empty . sets bhg) cmps
      -- The singletons which appear in the component of the empty
      -- set.
      r = Set.elems $ singletons bhg emptyCmp
      -- The singletons which appear in components, the union of sets
      -- of which is the full support.
      s = Set.elems $ Set.unions [ singletons bhg cmp
                                 | cmp <- cmps, Set.unions (sets bhg cmp) == supp ]

      (LabelledGraph consDepGr symbArr symbMap) = buildConsDepGraph bhg supp
      (dagGr, sccMap) = sccDAG consDepGr

      -- Find which vertices in the conservation dependency graph
      -- correspond to the symbols in 'r' and 's'.
      mappedR = map (symbMap Map.!) r
      mappedS = map (symbMap Map.!) s

      -- We have to remove from the conservation dependency graph all
      -- the connected components containing species from 'r',
      -- together with their descendants.  Find out which species
      -- exactly we need to remove.
      removeR = concatMap (descendants consDepGr) mappedR
      -- We also have to remove the connected components containing
      -- the species from 's'.  List the species we would need to
      -- remove in this case.
      removeS = concatMap (ancestors consDepGr) mappedS
      remove = removeR ++ removeS

      -- The indices of the SCC's to remove from the DAG induced by
      -- the SCC's of the conservation dependency graph.
      sccsToRemove = [ sccIdx | (sccIdx, vs) <- IntMap.assocs sccMap
                              , intersect remove vs /= [] ]

      -- Remove the unneeded SCC's from 'dagGr'.
      (reducedConsDepGr, redMap) = subgraph dagGr $ vertices dagGr \\ sccsToRemove

      ssets = sourceSetsDAG reducedConsDepGr

      -- Map the vertices of each source sets to the vertices of the
      -- full DAG induced by the SCC's of the conservation dependency
      -- graph, then find which vertices are contained in these SCC's,
      -- and finally the symbols associated with those vertices.
      consSetsRaw = map (map (symbArr Array.!)
                         . concatMap (sccMap IntMap.!)
                         . map (redMap IntMap.!)
                        ) ssets

      -- Add the symbols from 's' to all of the raw conserved sets.
  in [Set.empty] ++ map (Set.fromList . (++) s) consSetsRaw
