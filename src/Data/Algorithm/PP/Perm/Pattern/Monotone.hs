{-|
Module      : Data.Algorithm.PP.Perm.Pattern.Monotone
Description : Monotone patterns in permutations
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Computing monotone patterns in permutations.
-}

module Data.Algorithm.PP.Perm.Pattern.Monotone (
  -- * Ascending
    longestAscending

  -- * Descending
  , longestDescending
  ) where

import qualified Control.Arrow as A
import qualified Data.List     as L
import qualified Data.Tuple    as T
import qualified Patience

import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point
import qualified Data.Algorithm.PP.Perm           as PP.Perm

{- | 'longestAscending' @p@ returns a longest ascending pattern (as a list of points) of permutation @p@.

>>> longestAscending $ mk [4,9,1,2,7,5,6,3,8]
-}
longestAscending :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
longestAscending = L.map T.snd . L.reverse . Patience.longestIncreasing . L.map shape . PP.Perm.getPoints
  where
    shape = (A.&&&) PP.Geometry.Point.getY id . PP.Geometry.Point.getCoordinates

{- | 'longestDescending' @p@ returns a longest descending pattern (as a list of points) of permutation @p@.

>>> longestDescending $ mk [4,9,1,2,7,5,6,3,8]
-}
longestDescending :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
longestDescending = L.map T.snd . L.reverse . Patience.longestIncreasing . L.reverse . L.map shape . PP.Perm.getPoints
  where
    shape = (A.&&&) PP.Geometry.Point.getY id . PP.Geometry.Point.getCoordinates

