{-|
Module      : Data.Algorithm.PP.Perm.Component
Description : Combining permutations
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Combining permutations facilities.
-}

module Data.Algorithm.PP.Perm.Component (
    components
  ) where

import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T

import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point
import qualified Data.Algorithm.PP.Perm           as PP.Perm
import qualified Data.Algorithm.PP.Perm.Features  as PP.Perm.Features

{-| 'components' @p@ returns the components of permutation @p@
(/i.e./ the  factorizations @p = qr@, @q@ is non-empty, so that each element in @q@
is smaller than each element in @r@).

>>> let p = mk [2,1,3,5,4,6,8,7] in components p
[([2,1,3,5,4,6,8,7],[]),([2,1],[3,5,4,6,8,7]),([2,1,3],[5,4,6,8,7]),([2,1,3,5,4],[6,8,7]),([2,1,3,5,4,6],[8,7])]
>>> let p = mk [1..4] in components p
[([1,2,3,4],[]),([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]
>>> let p = mk [4,3,2,1] in components p
[([4,3,2,1],[])]
-}
components :: PP.Perm.Perm -> [([Int], [Int])]
components p = ((xs, []) :) . L.tail . F.foldr f [] $ PP.Perm.Features.leftToRightMaxima p
  where
    xs = PP.Perm.getList p

    f xy acc = L.splitAt (i-1) xs : acc
      where
        i = PP.Geometry.Point.getX xy
