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

components :: PP.Perm.Perm -> [([Int], [Int])]
components p = ((xs, []) :) . L.tail . F.foldr f [] $ PP.Perm.Features.leftToRightMaxima p
  where
    xs = PP.Perm.getList p

    f p acc = L.splitAt (i-1) xs : acc
      where
        i = PP.Geometry.Point.getX p
