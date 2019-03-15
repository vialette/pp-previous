{-|
Module      : Data.Algorithm.PP.Perm.Generator.Oscillating
Description : Generating oscillating permutations
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Generating oscillating permutations facilities.
-}

module Data.Algorithm.PP.Perm.Generator.Oscillating (
    oscillatingIncreasing
  , oscillatingIncreasings
  ) where

import qualified Data.Foldable as F
import qualified Data.List     as L

import qualified Data.Algorithm.PP.Perm as PP.Perm

{- | 'oscillatingIncreasing' @n@ return the oscillating increasing permutation of length @n@.

>>> oscillatingIncreasing 10
[3,1,5,2,7,4,9,6,10,8]
-}
oscillatingIncreasing :: Int-> PP.Perm.Perm
oscillatingIncreasing n = PP.Perm.mk . L.take n $ F.concat [[2*k+2, 2*k-1] | k <- [1..]]

{- | 'oscillatingIncreasings' return the infinite list of all oscillating increasing permutations.

>>> take 8 oscillatingIncreasings
[[1],[2,1],[2,1,3],[3,1,4,2],[3,1,4,2,5],[3,1,5,2,6,4],[3,1,5,2,6,4,7],[3,1,5,2,7,4,8,6]]
-}
oscillatingIncreasings :: [PP.Perm.Perm]
oscillatingIncreasings = [oscillatingIncreasing n | n <- [1..]]
