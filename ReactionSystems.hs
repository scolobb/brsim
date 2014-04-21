{- ReactionSystems.hs

Defines reaction systems and some of their basic dynamics.-}

module ReactionSystems ( Symbol(..)
                       , Symbols
                       , Reaction(..)
                       , Reactions
                       , ReactionSystem(..)
                       , makeReactionSystem
                       , enabled
                       , en
                       , applyOne
                       , apply
                       , res
                       , Context
                       , State
                       , Result
                       , InteractiveProcess(..)
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

enabled :: Reaction -> Symbols -> Bool
enabled (Reaction r i _) ss = (r `Set.isSubsetOf` ss) && (not (i `Set.isSubsetOf` ss))

en = enabled

applyOne :: Reaction -> Symbols -> Symbols
applyOne a@(Reaction _ _ p) ss | enabled a ss = p
applyOne _ _ | otherwise = Set.empty

apply :: Reactions -> Symbols -> Symbols
apply as ss = foldMap ((flip $ applyOne) ss) as

res = apply

type Context = Symbols
type State   = Symbols
type Result  = Symbols

data InteractiveProcess = InteractiveProcess { contexts       :: [Context]
                                             , results        :: [Result]
                                             } deriving (Show, Read, Eq)

stateSequence :: InteractiveProcess -> [State]
stateSequence (InteractiveProcess [] []) = []
stateSequence (InteractiveProcess (c0:cs) rs) = c0 : (map (uncurry Set.union) $ zip cs rs)

run :: ReactionSystem -> [Context] -> [Result]
run (ReactionSystem _ rs) = tail . reverse . foldl' (\res@(lastRes:_) ctx -> (apply rs $ lastRes `Set.union` ctx):res) [Set.empty]

run' :: ReactionSystem -> [Context] -> InteractiveProcess
run' rs ctx = InteractiveProcess ctx $ run rs ctx
