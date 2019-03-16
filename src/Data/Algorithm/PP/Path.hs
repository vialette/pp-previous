{-|
Module      : Data.Algorithm.PP.Path
Description : Paths
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

-}

module Data.Algorithm.PP.Path (
  -- * Type
    Path(..)

  -- * Constructing
  , mk
  ) where

import qualified Data.Foldable   as F
import qualified Data.List       as L

import qualified Data.Algorithm.PP.Path.Step as PP.Path.Step

{- Type definition -}
newtype Path = Path { getSteps :: [PP.Path.Step.Step] } deriving (Eq, Ord, Read)

--
instance Semigroup Path where
  p <> p' = mk (getSteps p ++ getSteps p')

--
instance Monoid Path where
    mempty  = mk []
    mappend = (<>)

--
instance Show Path where
  show = F.concatMap show . getSteps


{- | 'mk' @xs@ returns a path from a list of steps @xs@.

>>> mkUnsafe [UpStep, UpStep, DownStep, DownStep, UpStep, DownStep]
(())()
>>> mkUnsafe [DownStep, UpStep, DownStep, DownStep, UpStep, UpStep]
)())((
-}
mk :: [PP.Path.Step.Step] -> Path
mk ss = Path { getSteps = ss }

{- | 'empty' returns the empty path. -}
empty :: Path
empty = Path { getSteps = [] }