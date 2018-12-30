module Data.Algorithm.PP.Geometry.Point
(
  -- * Type
  Point

  -- * Construction
, mk
, mk'
, zero
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
, isDominatedBy
)
where

  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Tuple    as T

  -- |'Point' type
  newtype Point = Point { getCoordinates :: (Int, Int) } deriving (Eq, Ord)

  instance Show Point where
    show Point { getCoordinates = (x, y) } = "(" ++ show x ++ "," ++ show y ++ ")"

  -- |'mk' x' 'y' return the point with (x, y) coordinates.
  mk :: Int -> Int -> Point
  mk x y = Point { getCoordinates = (x, y) }

  mk' :: (Int, Int) -> Point
  mk' = uncurry mk

  -- |'zero' return the (0,0) point.
  zero :: Point
  zero = mk 0 0

  -- |'move' 'dx' 'dy' 'p' returns the point (x+dx, y+dy), where (x,y) are
  -- the coordinates of the point 'p'.
  move :: Int -> Int -> Point -> Point
  move dx dy p = mk (getX p + dx) (getY p + dy)

  -- |'symmetric' 'p' return the point with coordinates (y, x),
  -- where (x,y) are the coordinates of the point 'p'.
  symmetric :: Point -> Point
  symmetric Point { getCoordinates = (x, y) } = mk y x

  -- |'getX' 'p' return the x-coordinate of the point 'p'.
  getX :: Point -> Int
  getX = T.fst . getCoordinates

  -- |'getY' 'p' return the y-coordinate of the point 'p'.
  getY :: Point -> Int
  getY = T.snd . getCoordinates

  sortOn :: (Foldable t) => (Point -> Int) -> t Point -> [Point]
  sortOn f = L.sortOn f . F.toList

  -- |'sortOnX' 'ps' ascending sorts points 'ps' according to their x-ccordinates.
  sortOnX :: (Foldable t) => t Point -> [Point]
  sortOnX = sortOn getX

  -- |'sortOnDescX' 'ps' descending sorts points 'ps' according to their x-ccordinates.
  sortOnDescX :: (Foldable t) => t Point -> [Point]
  sortOnDescX = L.reverse . sortOnX

  -- |'sortOnY' 'ps' ascending sorts points 'ps' according to their y-ccordinates.
  sortOnY :: (Foldable t) => t Point -> [Point]
  sortOnY = sortOn getY

  -- |'sortOnDescY' 'ps' descending sorts points 'ps' according to their y-ccordinates.
  sortOnDescY :: (Foldable t) => t Point -> [Point]
  sortOnDescY = L.reverse . sortOnY

  -- |'isOnDiagonal' 'p' returns 'True' if @x == y@,
  -- where (x,y) are the coordinates of the point 'p'.
  isOnDiagonal :: Point -> Bool
  isOnDiagonal p = getX p == getY p

  -- |'isNotOnDiagonal' 'p' returns 'True' if @x /= y@,
  -- where (x,y) are the coordinates of the point 'p'.
  isNotOnDiagonal :: Point -> Bool
  isNotOnDiagonal = not . isOnDiagonal

  isStrictlyAboveDiagonal :: Point -> Bool
  isStrictlyAboveDiagonal p = getX p < getY p

  isAboveDiagonal :: Point -> Bool
  isAboveDiagonal p = getX p <= getY p

  isStrictlyBelowDiagonal :: Point -> Bool
  isStrictlyBelowDiagonal p = getX p > getY p

  isBelowDiagonal :: Point -> Bool
  isBelowDiagonal p = getX p >= getY p

  -- (@<-@) :: Point -> Point -> Bool
  -- p1 @<-@ p2 = getX p1 < getX p2

  isStrictlyOnTheLeftOf :: Point -> Point -> Bool
  p1 `isStrictlyOnTheLeftOf` p2 = getX p1 < getX p2

  -- (@<=-@) :: Point -> Point -> Bool
  -- p1 @<=-@ p2 = getX p1 <= getX p2

  isOnTheLeftOf :: Point -> Point -> Bool
  p1 `isOnTheLeftOf` p2 = getX p1 <= getX p2

  -- (@>-@) :: Point -> Point -> Bool
  -- p1 @>-@ p2 = getX p1 > getX p2

  isStrictlyOnTheRighttOf :: Point -> Point -> Bool
  p1 `isStrictlyOnTheRighttOf` p2 = getX p1 > getX p2

  -- (@>=-@) :: Point -> Point -> Bool
  -- p1 @>=-@ p2 = getX p1 >= getX p2

  isOnTheRightOf :: Point -> Point -> Bool
  p1 `isOnTheRightOf` p2 = getX p1 > getX p2

  -- (@<|@) :: Point -> Point -> Bool
  -- p1 @<|@ p2 = getY p1 < getY p2

  isStrictlyBelowOf :: Point -> Point -> Bool
  p1 `isStrictlyBelowOf` p2 = getY p1 < getY p2

  -- (@<=|@) :: Point -> Point -> Bool
  -- p1 @<=|@ p2 = getY p1 <= getY p2

  isBelowOf :: Point -> Point -> Bool
  p1 `isBelowOf` p2 = getY p1 <= getY p2

  -- (@>|@) :: Point -> Point -> Bool
  -- p1 @>|@ p2 = getY p1 > getY p2

  isStrictlyAboveOf :: Point -> Point -> Bool
  p1 `isStrictlyAboveOf` p2 = getY p1 > getY p2

  -- (@>=|@) :: Point -> Point -> Bool
  -- p1 @>=|@ p2 = getY p1 >= getY p2

  isAboveOf :: Point -> Point -> Bool
  p1 `isAboveOf` p2 = getY p1 >= getY p2

  isStrictlyDominatedBy :: Point -> Point -> Bool
  p1 `isStrictlyDominatedBy` p2 = getX p1 < getX p2 && getY p1 < getY p2

  isDominatedBy :: Point -> Point -> Bool
  p1 `isDominatedBy` p2 = getX p1 <= getX p2 && getY p1 <= getY p2
