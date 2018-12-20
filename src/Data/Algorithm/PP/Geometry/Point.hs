module Data.Algorithm.PP.Geometry.Point
(
  -- * Type
  Point

  -- * Construction
, mk
, mk'
, symmetric

  -- * Querying
, getCoordinates
, getX
, getY

  -- * Sort
, sortOn
, sortOnX
, sortOnY

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
, isDominatedBy
)
where

  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Tuple    as T

  -- |'Point' type
  newtype Point a = Point { getCoordinates :: (a, a) } deriving (Eq, Ord)

  instance (Show a) => Show (Point a) where
    show Point { getCoordinates = (x, y) } = "(" ++ show x ++ "," ++ show y ++ ")"

  mk :: a -> a -> Point a
  mk x y = Point { getCoordinates = (x, y) }

  mk' :: (a, a) -> Point a
  mk' = uncurry mk

  symmetric :: Point a -> Point a
  symmetric Point { getCoordinates = (x, y) } = mk y x

  getX :: Point a -> a
  getX = T.fst . getCoordinates

  getY :: Point a -> a
  getY = T.snd . getCoordinates

  sortOn :: (Foldable t, Ord b) => (Point a -> b) -> t (Point a) -> [Point a]
  sortOn f = L.sortOn f . F.toList

  sortOnX :: (Foldable t, Ord a) => t (Point a) -> [Point a]
  sortOnX = sortOn getX

  sortOnY :: (Foldable t, Ord a) => t (Point a) -> [Point a]
  sortOnY = sortOn getY

  isOnDiagonal :: (Eq a) => Point a -> Bool
  isOnDiagonal p = getX p == getY p

  isNotOnDiagonal :: (Eq a) => Point a -> Bool
  isNotOnDiagonal = not . isOnDiagonal

  isStrictlyAboveDiagonal :: (Ord a) => Point a -> Bool
  isStrictlyAboveDiagonal p = getX p < getY p

  isAboveDiagonal :: (Ord a) => Point a -> Bool
  isAboveDiagonal p = getX p <= getY p

  isStrictlyBelowDiagonal :: (Ord a) => Point a -> Bool
  isStrictlyBelowDiagonal p = getX p > getY p

  isBelowDiagonal :: (Ord a) => Point a -> Bool
  isBelowDiagonal p = getX p >= getY p

  -- (@<-@) :: Point a -> Point a -> Bool
  -- p1 @<-@ p2 = getX p1 < getX p2

  isStrictlyOnTheLeftOf :: (Ord a) => Point a -> Point a -> Bool
  p1 `isStrictlyOnTheLeftOf` p2 = getX p1 < getX p2

  -- (@<=-@) :: Point a -> Point a -> Bool
  -- p1 @<=-@ p2 = getX p1 <= getX p2

  isOnTheLeftOf :: (Ord a) => Point a -> Point a -> Bool
  p1 `isOnTheLeftOf` p2 = getX p1 <= getX p2

  -- (@>-@) :: Point a -> Point a -> Bool
  -- p1 @>-@ p2 = getX p1 > getX p2

  isStrictlyOnTheRighttOf :: (Ord a) => Point a -> Point a -> Bool
  p1 `isStrictlyOnTheRighttOf` p2 = getX p1 > getX p2

  -- (@>=-@) :: Point a -> Point a -> Bool
  -- p1 @>=-@ p2 = getX p1 >= getX p2

  isOnTheRightOf :: (Ord a) => Point a -> Point a -> Bool
  p1 `isOnTheRightOf` p2 = getX p1 > getX p2

  -- (@<|@) :: Point a -> Point a -> Bool
  -- p1 @<|@ p2 = getY p1 < getY p2

  isStrictlyBelowOf :: (Ord a) => Point a -> Point a -> Bool
  p1 `isStrictlyBelowOf` p2 = getY p1 < getY p2

  -- (@<=|@) :: Point a -> Point a -> Bool
  -- p1 @<=|@ p2 = getY p1 <= getY p2

  isBelowOf :: (Ord a) => Point a -> Point a -> Bool
  p1 `isBelowOf` p2 = getY p1 <= getY p2

  -- (@>|@) :: Point a -> Point a -> Bool
  -- p1 @>|@ p2 = getY p1 > getY p2

  isStrictlyAboveOf :: (Ord a) => Point a -> Point a -> Bool
  p1 `isStrictlyAboveOf` p2 = getY p1 > getY p2

  -- (@>=|@) :: Point a -> Point a -> Bool
  -- p1 @>=|@ p2 = getY p1 >= getY p2

  isAboveOf :: (Ord a) => Point a -> Point a -> Bool
  p1 `isAboveOf` p2 = getY p1 >= getY p2

  isStrictlyDominatedBy :: (Ord a) => Point a -> Point a -> Bool
  p1 `isStrictlyDominatedBy` p2 = getX p1 < getX p2 && getY p1 < getY p2

  isDominatedBy :: (Ord a) => Point a -> Point a -> Bool
  p1 `isDominatedBy` p2 = getX p1 <= getX p2 && getY p1 <= getY p2
