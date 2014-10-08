{- ReactionSystems.hs

Defines reaction systems and some of their basic dynamics.

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

module ReactionSystems ( Symbol(..)
                       , Symbols
                       , Reaction(..)
                       , Reactions
                       , ReactionSystem(..)
                       , makeReactionSystem
                       , makeReactionSystem'
                       , support
                       , enabled
                       , en
                       , applyOne
                       , apply
                       , res
                       , applyRS
                       , Context
                       , State
                       , Result
                       , InteractiveProcess(..)
                       , makeInteractiveProcess
                       , stateSequence
                       , run
                       , run'
                       ) where

import qualified Data.Set as Set
import Data.Foldable (foldMap)
import Data.List (foldl')

newtype Symbol = Symbol { name :: String } deriving (Show, Read, Ord, Eq)

type Symbols = Set.Set Symbol

data Reaction = Reaction { reactants  :: Symbols
                         , inhibitors :: Symbols
                         , products   :: Symbols
                         } deriving (Show, Read, Ord, Eq)

type Reactions = Set.Set Reaction

data ReactionSystem = ReactionSystem { symbols   :: Symbols
                                     , reactions :: Reactions
                                     } deriving (Show, Read, Eq)

makeReactionSystem :: [Reaction] -> ReactionSystem
makeReactionSystem rs = let reactions = Set.fromList rs
                            symbols = foldMap listSymbols reactions
                        in ReactionSystem symbols reactions
  where listSymbols (Reaction r i p) = r `Set.union` i `Set.union` p

makeReactionSystem' :: Reactions -> ReactionSystem
makeReactionSystem' = makeReactionSystem . Set.toList

support :: ReactionSystem -> Symbols
support (ReactionSystem _ rs) = Set.unions $ uncurry (++) $ unzip
                                $ map (\(Reaction r _ p) -> (r, p)) $ Set.toList rs

enabled :: Reaction -> Symbols -> Bool
enabled (Reaction r i _) ss = (r `Set.isSubsetOf` ss) && (Set.null i || Set.null (i `Set.intersection` ss))

en = enabled

applyOne :: Reaction -> Symbols -> Symbols
applyOne a@(Reaction _ _ p) ss | enabled a ss = p
applyOne _ _ | otherwise = Set.empty

apply :: Reactions -> Symbols -> Symbols
apply as ss = foldMap ((flip applyOne) ss) as

res = apply

applyRS :: ReactionSystem -> Symbols -> Symbols
applyRS (ReactionSystem _ rs) = apply rs

type Context = Symbols
type State   = Symbols
type Result  = Symbols

data InteractiveProcess = InteractiveProcess { contexts       :: [Context]
                                             , results        :: [Result]
                                             } deriving (Show, Read, Eq)

makeInteractiveProcess :: [Context] -> [Result] -> InteractiveProcess
makeInteractiveProcess ctx res = InteractiveProcess ctx $ Set.empty:res

stateSequence :: InteractiveProcess -> [State]
stateSequence (InteractiveProcess [] []) = []
stateSequence (InteractiveProcess (c0:cs) rs) = c0 : (map (uncurry Set.union) $ zip cs rs)

run :: ReactionSystem -> [Context] -> [Result]
run (ReactionSystem _ rs) = tail . reverse . foldl' (\res@(lastRes:_) ctx -> (apply rs $ lastRes `Set.union` ctx):res) [Set.empty]

run' :: ReactionSystem -> [Context] -> InteractiveProcess
run' rs ctx = InteractiveProcess ctx $ run rs ctx
