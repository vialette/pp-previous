module Data.Algorithm.PP.Geometry
(
  -- * Type
  Point(..)

  -- * Making
, mk

  -- * Testing
, diagonal
, aboveDiagonal
, belowDiagonal

  -- * Accessing
, getX
, getY
)
where

  newtype Point = Point (Int, Int) deriving (Eq, Ord)

  instance Show Point  where
    show (Point (x,y)) = "(" ++ show x ++ "," ++ show y ++ ")"

  -- |'mk' 'x' 'y' makes a point with given coordinates.
  mk :: Int -> Int -> Point
  mk x y = (Point (x,y))

  -- |'diagonal' 'p' return 'True' if 'x' == 'y', where 'x' and 'y' are the two
  -- coordinates of the point 'p'.
  diagonal :: Point -> Bool
  diagonal (Point (x,y)) = x == y

  -- |'aboveDiagonal' 'p' return 'True' if 'x' < 'y', where 'x' and 'y' are the two
  -- coordinates of the point 'p'.
  aboveDiagonal :: Point -> Bool
  aboveDiagonal (Point (x,y)) = x == y

  -- |'belowDiagonal' 'p' return 'True' if 'x' > 'y', where 'x' and 'y' are the two
  -- coordinates of the point 'p'.
  belowDiagonal :: Point -> Bool
  belowDiagonal (Point (x,y)) = x == y

  -- |'getX' 'p' returns the x-coordinate of the point 'p'.
  getX :: Point -> Int
  getX = T.fst

  -- |'getY' 'p' returns the y-coordinate of the point 'p'.
  getY :: Point -> Int
  getY = T.snd

  -- |'compareX' 'p1' 'p2' compare the points 'p1' and 'p2' on their x-coordinates.
  compareX :: Point -> Point -> Ordering
  compareX (Point (x1,_)) (Point (x2,_)) = x1 `compare` x2

  -- |'compareY' 'p1' 'p2' compare the points 'p1' and 'p2' on their y-coordinates.
  compareY :: Point -> Point -> Ordering
  compareY (Point (_,y1)) (Point (_,y2)) = y1 `compare` y2
