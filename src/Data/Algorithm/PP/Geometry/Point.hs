module Data.Algorithm.PP.Geometry.Point
(
  -- * Type
  Point(..)

  -- * Making
, mk

  -- * Comparing
, (@<-)
, (@>-)
, (@<|)
, (@>|)

  -- * Testing
, diagonal
, aboveDiagonal
, belowDiagonal

  -- * Accessing
, getX
, getY
)
where

  newtype Point = Point { getCoordinates :: (Int, Int) } deriving (Eq, Ord)

  instance Show Point  where
    show Point { getCoordinates = (x,y) } = "(" ++ show x ++ "," ++ show y ++ ")"

  -- |'mk' 'x' 'y' makes a point with given coordinates.
  mk :: Int -> Int -> Point
  mk x y = Point { getCoordinates = (x,y) }

  -- |'diagonal' 'p' return 'True' if 'x' == 'y', where 'x' and 'y' are the two
  -- coordinates of the point 'p'.
  diagonal :: Point -> Bool
  diagonal Point { getCoordinates = (x,y) } = x == y

  -- |'aboveDiagonal' 'p' return 'True' if 'x' < 'y', where 'x' and 'y' are the two
  -- coordinates of the point 'p'.
  aboveDiagonal :: Point -> Bool
  aboveDiagonal Point { getCoordinates = (x,y) } = x == y

  -- |'belowDiagonal' 'p' return 'True' if 'x' > 'y', where 'x' and 'y' are the two
  -- coordinates of the point 'p'.
  belowDiagonal :: Point -> Bool
  belowDiagonal Point { getCoordinates = (x,y) } = x == y

  -- |'getX' 'p' returns the x-coordinate of the point 'p'.
  getX :: Point -> Int
  getX Point { getCoordinates = (x,_) } = x

  -- |'getY' 'p' returns the y-coordinate of the point 'p'.
  getY :: Point -> Int
  getY Point { getCoordinates = (_,y) } = y

  -- |'compareX' 'p1' 'p2' compare the points 'p1' and 'p2' on their x-coordinates.
  compareX :: Point -> Point -> Ordering
  compareX p1 p2 = getX p1 `compare` getX p2

  (@<-) :: Point -> Point -> Bool
  p1 @<- p2 = getX p1 < getX p2

  (@>-) :: Point -> Point -> Bool
  p1 @>- p2 = getX p1 > getX p2

  -- |'compareY' 'p1' 'p2' compare the points 'p1' and 'p2' on their y-coordinates.
  compareY :: Point -> Point -> Ordering
  compareY p1 p2 = getX p1 `compare` getX p2

  (@<|) :: Point -> Point -> Bool
  p1 @<| p2 = getY p1 < getY p2

  (@>|) :: Point -> Point -> Bool
  p1 @>| p2 = getY p1 > getY p2
