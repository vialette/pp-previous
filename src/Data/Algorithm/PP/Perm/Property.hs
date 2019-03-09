{-|
Module      : Data.Algorithm.PP.Perm.Property
Description :
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PP.Perm.Property
  (
    derangement

  -- * Monotone
  , increasing
  , decreasing
  , monotone

  -- Alternating
  , upDownAlternating
  , downUpAlternating
  , alternating
  ) where

import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T

import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point
import qualified Data.Algorithm.PP.Perm           as PP.Perm
import qualified Data.Algorithm.PP.Utils.List     as PP.Utils.List

{- | 'derangement' @@p@ returns @True@ if the permutation @p@ is a derangement
(i.e. @p@ is a permutation that has no fixed points).
-}
derangement :: PP.Perm.Perm -> Bool
derangement = F.all PP.Geometry.Point.isNotOnDiagonal . PP.Perm.getPoints

{- | 'increasing' @p@ returns @True@ if the permutation @p@ is increasing. -}
increasing :: PP.Perm.Perm -> Bool
increasing = F.any (uncurry (<)) . PP.Utils.List.chunk2 . PP.Perm.getList

{- | 'decreasing' @p@ returns @True@ if the permutation @p@ is decreasing. -}
decreasing :: PP.Perm.Perm -> Bool
decreasing = F.any (uncurry (>)) . PP.Utils.List.chunk2 . PP.Perm.getList

{- | 'monotone' @p@ returns @True@ if the permutation @p@ is monotone
(i.e. @p@ is either increasing or decreasing).
-}
monotone :: PP.Perm.Perm -> Bool
monotone p = increasing p || decreasing p

-- |
upDownAlternating' :: [(Int, Int, Int)] -> Bool
upDownAlternating' []                 = True
upDownAlternating' ((i, j, k) : ijks) = i < j && j > k && downUpAlternating' ijks

-- |
downUpAlternating' :: [(Int, Int, Int)] -> Bool
downUpAlternating' []                 = True
downUpAlternating' ((i, j, k) : ijks) = i > j && j < k && upDownAlternating' ijks

{- | 'upDownAlternating' @p@ return @True@ if the permutation @p@ is alternating
and starts with an up-step.

>>> upDownAlternating $ mk [3,5,2,4,1]
True
>>> upDownAlternating $ mk [4,1,5,2,3]
False
-}
upDownAlternating :: PP.Perm.Perm -> Bool
upDownAlternating = upDownAlternating' . PP.Utils.List.chunk3 . PP.Perm.getList

{- | 'downUpAlternating' @p@ return @True@ if the permutation @p@ is alternating
and starts with an down-step.

>>> downUpAlternating $ mk [3,5,2,4,1]
False
>>> downUpAlternating $ mk [4,1,5,2,3]
True
-}
downUpAlternating :: PP.Perm.Perm -> Bool
downUpAlternating = downUpAlternating' . PP.Utils.List.chunk3 . PP.Perm.getList

{- | 'alternating' @p@ return @True@ if the permutation @p@ is alternating
(i.e. each entry of @p@ is alternately greater or less than the preceding entry).

>>> alternating $ mk [3,5,2,4,1]
True
>>> alternating $ mk [4,1,5,2,3]
True
-}
alternating :: PP.Perm.Perm -> Bool
alternating p = upDownAlternating p || downUpAlternating p
