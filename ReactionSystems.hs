{- ReactionSystems.hs

Defines reaction systems and some of their basic dynamics.-}

module ReactionSystems ( Symbol(..)
                       , Symbols
                       ) where

import qualified Data.Set as Set
import Data.Foldable (foldMap)

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
                                             , results        :: [Context]
                                             } deriving (Show, Read, Eq)
