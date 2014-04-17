{- ReactionSystems.hs

Defines reaction systems and some of their basic dynamics.-}

module ReactionSystems ( Symbol(..)
                       , name
                       , Symbols
                       ) where

import qualified Data.Set as Set

newtype Symbol = Symbol { name :: String } deriving (Show, Read, Ord, Eq)

type Symbols = Set.Set Symbol

data Reaction = Reaction { reactants :: Symbols
                         , inhibitors :: Symbols
                         , products :: Symbols
                         } deriving (Show, Read, Ord, Eq)

type Reactions = Set.Set Reaction

data ReactionSystem = ReactionSystem { symbols :: Symbols
                                     , reactions :: Reactions
                                     } deriving (Show, Read, Eq)
