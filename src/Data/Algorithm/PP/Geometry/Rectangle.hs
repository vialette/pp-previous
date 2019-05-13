{-|
Module      : Data.Algorithm.PP.Geometry.Rectangle
Description : 2D rectangles
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental


-}

module Data.Algorithm.PP.Geometry.Rectangle (
  -- * Type
    Rectangle(..)

  -- * Constructing
  , mk
  , mk'

  -- * Transforming
  , move

  -- * Querying
  , width
  , height
  , minX
  , maxX
  , minY
  , maxY
  , insideX
  , insideY
  , inside
  , intersect
  , intersection
  ) where

import qualified Data.Tuple as T

import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point
import qualified Data.Algorithm.PP.Utils.Maybe    as PP.Utils.Maybe

newtype Rectangle = Rectangle { getPoints :: (PP.Geometry.Point.Point, PP.Geometry.Point.Point) } deriving (Eq, Ord)

instance Show Rectangle where
  show r = "(" ++ show (lowerLeftPoint r) ++ "," ++ show (upperRightPoint r) ++ ")"

mk :: PP.Geometry.Point.Point -> PP.Geometry.Point.Point -> Maybe Rectangle
mk p1 p2 = PP.Utils.Maybe.whenMaybe (p1 `PP.Geometry.Point.isOnTheLeftOf` p2 && p1 `PP.Geometry.Point.isBelowOf` p2) r
  where
    r = Rectangle { getPoints = (p1, p2) }

mk' :: PP.Geometry.Point.Point -> PP.Geometry.Point.Point -> Rectangle
mk' p1 p2 = Rectangle { getPoints = (pLowerLeft, pUpperRight) }
  where
    pLowerLeft = PP.Geometry.Point.mk x y
      where
        x = min (PP.Geometry.Point.getX p1) (PP.Geometry.Point.getX p2)
        y = min (PP.Geometry.Point.getY p1) (PP.Geometry.Point.getY p2)
    pUpperRight = PP.Geometry.Point.mk x y
      where
        x = max (PP.Geometry.Point.getX p1) (PP.Geometry.Point.getX p2)
        y = max (PP.Geometry.Point.getY p1) (PP.Geometry.Point.getY p2)

lowerLeftPoint :: Rectangle -> PP.Geometry.Point.Point
lowerLeftPoint = T.fst . getPoints

upperRightPoint :: Rectangle -> PP.Geometry.Point.Point
upperRightPoint = T.snd . getPoints

lowerRightPoint :: Rectangle -> PP.Geometry.Point.Point
lowerRightPoint r = PP.Geometry.Point.mk x y
  where
    x = PP.Geometry.Point.getX $ upperRightPoint r
    y = PP.Geometry.Point.getY $ lowerLeftPoint r

upperLeftPoint :: Rectangle -> PP.Geometry.Point.Point
upperLeftPoint r = PP.Geometry.Point.mk x y
  where
    x = PP.Geometry.Point.getX $ lowerLeftPoint r
    y = PP.Geometry.Point.getY $ upperRightPoint r

{- | 'minX' @r@ returns the y-coordinate of the lower left point of rectangle @r@. -}
minX :: Rectangle -> Int
minX = PP.Geometry.Point.getX . lowerLeftPoint

{- | 'maxX' @r@ returns the x-coordinate of the upper right point of rectangle @r@. -}
maxX :: Rectangle -> Int
maxX = PP.Geometry.Point.getX . upperRightPoint

{- | 'minY' @r@ returns the y-coordinate of the lower left point of rectangle @r@. -}
minY :: Rectangle -> Int
minY = PP.Geometry.Point.getY . lowerLeftPoint

{- | 'maxY' @r@ returns the y-coordinate of the upper right point of rectangle @r@. -}
maxY :: Rectangle -> Int
maxY = PP.Geometry.Point.getY . upperRightPoint

move :: Int -> Int -> Rectangle -> Rectangle
move dx dy r = Rectangle { getPoints = (lowerLeft, upperRight) }
  where
    lowerLeft  = PP.Geometry.Point.move dx dy $ lowerLeftPoint r
    upperRight = PP.Geometry.Point.move dx dy $ upperRightPoint r

width :: Rectangle -> Int
width r = maxX r - minX r

height :: Rectangle -> Int
height r = maxY r - minY r

insideX :: PP.Geometry.Point.Point -> Rectangle -> Bool
p `insideX` r = minX r <= x && x <= maxX r
  where
    x = PP.Geometry.Point.getX p

insideY :: PP.Geometry.Point.Point -> Rectangle -> Bool
p `insideY` r = minY r <= y && y <= maxY r
  where
    y = PP.Geometry.Point.getY p

inside :: PP.Geometry.Point.Point -> Rectangle -> Bool
p `inside` r = p `insideX` r && p `insideY` r

intersect :: Rectangle -> Rectangle -> Bool
intersect r1 r2 = case intersection r1 r2 of
  Nothing -> False
  Just _  -> True

intersection :: Rectangle -> Rectangle -> Maybe Rectangle
intersection r1 r2
  | maxX r1 < minX r2 || maxX r2 < minX r1 = Nothing
  | maxY r1 < minY r2 || maxY r2 < minY r1 = Nothing
  | otherwise                              = mk lowerLeft upperRight
    where
      lowerLeft = PP.Geometry.Point.mk x y
        where
          x = max (minX r1) (minX r2)
          y = max (minY r1) (minY r2)
      upperRight = PP.Geometry.Point.mk x y
         where
           x = min (maxX r1) (maxX r2)
           y = min (maxY r1) (maxY r2)

boundingRectangle :: Rectangle -> Rectangle -> Rectangle
boundingRectangle r1 r2 = mk' pLowerLeft pUpperRight
  where
    pLowerLeft  = min (lowerLeftPoint r1)  (lowerLeftPoint r2)
    pUpperRight = max (upperRightPoint r1) (upperRightPoint r2)