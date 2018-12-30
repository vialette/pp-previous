module Data.Algorithm.PP.Path.Internal
  (
    Path(..)
  ) where

-- |Type defintiion
newtype Path a = Path { getSteps :: [PP.Path.Step.Step a] } deriving (Eq, Ord)
