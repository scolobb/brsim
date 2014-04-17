{- ReactionSystems.hs

Defines reaction systems and some of their basic dynamics.-}

module ReactionSystems ( Symbol(..)
                       , name
                       ) where

newtype Symbol = Symbol { name :: String } deriving (Show, Read, Ord, Eq)
