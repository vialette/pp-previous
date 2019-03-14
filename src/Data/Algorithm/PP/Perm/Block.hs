module Data.Algorithm.PP.Perm.Block (

  ) where

import qualified Data.List as L

import qualified Data.Algorithm.PP.Geometry.Point     as PP.Geometry.Point
import qualified Data.Algorithm.PP.Geometry.Rectangle as PP.Geometry.Rectangle
import qualified Data.Algorithm.PP.Perm               as PP.Perm

mk :: PP.Geometry.Rectangle.Rectangle -> PP.Perm.Perm -> PP.Perm.Perm
mk r = PP.Perm.mk . L.map PP.Geometry.Point.getY . filterY . filterX . PP.Perm.getPoints
  where
    minX = PP.Geometry.Rectangle.minX r
    maxX = PP.Geometry.Rectangle.maxX r

    minY = PP.Geometry.Rectangle.minY r
    maxY = PP.Geometry.Rectangle.maxY r

    filterX = L.takeWhile (\p -> PP.Geometry.Point.getX p <= maxX) . L.drop (PP.Geometry.Rectangle.minX r)
    filterY = L.filter (\ p -> PP.Geometry.Point.getY p >= minY && PP.Geometry.Point.getY p <= maxY)
