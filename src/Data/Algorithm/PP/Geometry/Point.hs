{-|
Module      : Data.Algorithm.PP.Geometry.Point
Description : 2D points
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental


-}

module Data.Algorithm.PP.Geometry.Point (
    -- * Type
    Point

    -- * Construction
  , mk
  , mk'
  , mkZero
  , mkOnDiagonal
  , move
  , symmetric

    -- * Querying
  , getCoordinates
  , getX
  , getY

    -- * Sort
  , sortOn
  , sortOnX
  , sortOnDescX
  , sortOnY
  , sortOnDescY

    -- * Locate
  , isOnDiagonal
  , isNotOnDiagonal
  , isStrictlyAboveDiagonal
  , isAboveDiagonal
  , isStrictlyBelowDiagonal
  , isBelowDiagonal

    -- * Compare
  , isStrictlyOnTheLeftOf
  , isOnTheLeftOf
  , isStrictlyOnTheRighttOf
  , isOnTheRightOf
  , isStrictlyBelowOf
  , isBelowOf
  , isStrictlyAboveOf
  , isAboveOf
  , isStrictlyDominatedBy
  , dominatedBy
  ) where

import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T

{- | 'Point' type. -}
newtype Point = Point { getCoordinates :: (Int, Int) } deriving (Eq, Ord)

instance Show Point where
  show Point { getCoordinates = (x, y) } = "(" ++ show x ++ "," ++ show y ++ ")"

{- | 'mk' @x@ @y@ return a point with (x, y) coordinates.

>>> mk 2 3
-}
mk :: Int -> Int -> Point
mk x y = Point { getCoordinates = (x, y) }

{- | 'mk'' @(x, y)@ return a point with (x, y) coordinates.

>>> mk' (2, 3)
-}
mk' :: (Int, Int) -> Point
mk' = uncurry mk

{- | 'mkZero' return the point at the origin.

>>> mkZero
(0,0)

-}
mkZero :: Point
mkZero = mk 0 0

{- | 'onDiagonal' @i@ returns the point @(i, i)@. -}
mkOnDiagonal :: Int -> Point
mkOnDiagonal i = mk i i

{- | 'move' @dx@ @dy@ @p@ returns the point @(x+dx, y+dy)@, where @(x,y)@ are the coordinates of the point @p@.

>>> move 2 3 $ mk 4 5
(6,8)
-}
move :: Int -> Int -> Point -> Point
move dx dy p = mk (getX p + dx) (getY p + dy)

{- | 'symmetric' @p@ returns the point with coordinates @(y, x)@, where @(x,y)@ are the coordinates of the point @p@.

>>> symmetric $ mk 4 5
(5,4)
-}
symmetric :: Point -> Point
symmetric Point { getCoordinates = (x, y) } = mk y x

{- | 'getX' @p@ returns the @x@-coordinate of the point @p@. -}
getX :: Point -> Int
getX = T.fst . getCoordinates

{- | 'getY' @p@ returns the @y@-coordinate of the point @p@. -}
getY :: Point -> Int
getY = T.snd . getCoordinates

{- | 'sortOn' @f@ @ps@ sorts the points @ps@ according to the function @f@. -}
sortOn :: (Foldable t) => (Point -> Int) -> t Point -> [Point]
sortOn f = L.sortOn f . F.toList

{- | 'sortOnX' @ps@ ascending sorts points @ps@ according to their @x@-coordinates.

>>> sortOnX [mk i j | i <- [3,2,1], j <- [1..3]]
[(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
-}
sortOnX :: (Foldable t) => t Point -> [Point]
sortOnX = sortOn getX

{- | 'sortOnDescX' @ps@ descending sorts points @ps@ according to their @x@-coordinates.

>>> sortOnDescX [mk i j | i <- [1..3], j <- [1..3]]
[(3,3),(3,2),(3,1),(2,3),(2,2),(2,1),(1,3),(1,2),(1,1)]
-}
sortOnDescX :: (Foldable t) => t Point -> [Point]
sortOnDescX = L.reverse . sortOnX

{- | 'sortOnY' @ps@ ascending sorts points @ps@ according to their @y@-coordinates.

>>> sortOnY [mk i j | i <- [1..3], j <- [1..3]]
[(1,1),(2,1),(3,1),(1,2),(2,2),(3,2),(1,3),(2,3),(3,3)]
-}
sortOnY :: (Foldable t) => t Point -> [Point]
sortOnY = sortOn getY

{- | 'sortOnDescY' @ps@ descending sorts points @ps@ according to their @y@-coordinates.

>>> sortOnDescY [mk i j | i <- [1..3], j <- [1..3]]
[(3,3),(2,3),(1,3),(3,2),(2,2),(1,2),(3,1),(2,1),(1,1)]
-}
sortOnDescY :: (Foldable t) => t Point -> [Point]
sortOnDescY = L.reverse . sortOnY

{- | 'isOnDiagonal' @p@ returns @True@ if @x == y@, where @(x,y)@ are the coordinates of the point @p@. -}
isOnDiagonal :: Point -> Bool
isOnDiagonal p = getX p == getY p

{- | 'isOnDiagonal' @p@ returns @True@ if @x /= y@, where @(x,y)@ are the coordinates of the point @p@. -}
isNotOnDiagonal :: Point -> Bool
isNotOnDiagonal = not . isOnDiagonal

{- | 'isStrictlyAboveDiagonal' @p@ returns @True@ if @x < y@, where @(x,y)@ are the coordinates of the point @p@. -}
isStrictlyAboveDiagonal :: Point -> Bool
isStrictlyAboveDiagonal p = getX p < getY p

{- | 'isAboveDiagonal' @p@ returns @True@ if @x <= y@, where @(x,y)@ are the coordinates of the point @p@. -}
isAboveDiagonal :: Point -> Bool
isAboveDiagonal p = getX p <= getY p

{- | 'isStrictlyBelowDiagonal' @p = (x, y)@ returns @True@ if @x > y@. -}
isStrictlyBelowDiagonal :: Point -> Bool
isStrictlyBelowDiagonal p = getX p > getY p

{- | 'isBelowDiagonal' @p = (x, y)@ returns @True@ if @x >= y@. -}
isBelowDiagonal :: Point -> Bool
isBelowDiagonal p = getX p >= getY p

{- | 'isOnTheLeftOf' @p1 = (x1, y1)@ @p2 = (x2, y2)@ returns @True@ if @x1 <= x2@. -}
isOnTheLeftOf :: Point -> Point -> Bool
p1 `isOnTheLeftOf` p2 = getX p1 <= getX p2

isStrictlyOnTheRighttOf :: Point -> Point -> Bool
p1 `isStrictlyOnTheRighttOf` p2 = getX p1 > getX p2

isOnTheRightOf :: Point -> Point -> Bool
p1 `isOnTheRightOf` p2 = getX p1 > getX p2

isStrictlyBelowOf :: Point -> Point -> Bool
p1 `isStrictlyBelowOf` p2 = getY p1 < getY p2

isBelowOf :: Point -> Point -> Bool
p1 `isBelowOf` p2 = getY p1 <= getY p2

isStrictlyAboveOf :: Point -> Point -> Bool
p1 `isStrictlyAboveOf` p2 = getY p1 > getY p2

isAboveOf :: Point -> Point -> Bool
p1 `isAboveOf` p2 = getY p1 >= getY p2

isStrictlyDominatedBy :: Point -> Point -> Bool
p1 `isStrictlyDominatedBy` p2 = getX p1 < getX p2 && getY p1 < getY p2

{- | 'dominatedBy' @p1@ @p2@ returns @True@ if @x1 <= x2@ and @y1 <= y2, where
 @(x1, y1)@ and (@x2, y2)@ are the coordinates of the points @p1@ and @p2@, respectively.

>>> let p1 = mk 1 2; p2 = mk 2 3 in dominatedBy p1 p2
True
>>> let p1 = mk 1 2; p2 = mk 4 1 in dominatedBy p1 p2
False
>>> let p = mk 1 2 in dominatedBy p p
True
-}
dominatedBy :: Point -> Point -> Bool
p1 `dominatedBy` p2 = getX p1 <= getX p2 && getY p1 <= getY p2
