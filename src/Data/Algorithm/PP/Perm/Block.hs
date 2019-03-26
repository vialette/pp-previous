{-|
Module      : Data.Algorithm.PP.Block
Description : Paths
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

-}

module Data.Algorithm.PP.Perm.Block (
    mk
  ) where

import qualified Data.List as L

import qualified Data.Algorithm.PP.Geometry.Point     as PP.Geometry.Point
import qualified Data.Algorithm.PP.Geometry.Rectangle as PP.Geometry.Rectangle
import qualified Data.Algorithm.PP.Perm               as PP.Perm

{- | 'block' @r@ @p@ returns the permutation obtained by reducing the elements in rectangle @r@.

>>> p = Perm.mk [3,1,2,7,6,8,5,4]
>>> Rectangle.mk (Point.mk 1 1) (Point.mk 9 9) >>= Just . (\ r -> mk r p)
Just [3,1,9,2,7,6,8,5,4]
>>> Rectangle.mk (Point.mk 2 3) (Point.mk 7 8) >>= Just . (\ r -> mk r p)
Just [3,2,4,1]
-}
mk :: PP.Geometry.Rectangle.Rectangle -> PP.Perm.Perm -> PP.Perm.Perm
mk r = PP.Perm.mk . L.map PP.Geometry.Point.getY . filterY . filterX . PP.Perm.getPoints
  where
    minX = PP.Geometry.Rectangle.minX r
    maxX = PP.Geometry.Rectangle.maxX r

    minY = PP.Geometry.Rectangle.minY r
    maxY = PP.Geometry.Rectangle.maxY r

    filterX = L.takeWhile (\p -> PP.Geometry.Point.getX p <= maxX) . L.drop (PP.Geometry.Rectangle.minX r - 1)
    filterY = L.filter (\ p -> PP.Geometry.Point.getY p >= minY && PP.Geometry.Point.getY p <= maxY)
