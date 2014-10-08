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
                  ) where

import ReactionSystems
import qualified Data.Set as Set
import Data.List (subsequences)

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
