module Data.Algorithm.PP.Geometry
(
  -- * Type
  Point

  -- * Making
, mk

  -- * Testing
, diagonal

  -- * Accessing
, getX
, getY
)
where

  newtype Point = Point { getPoint (Int, Int) } deriving (Eq, Ord)

  instance Show Point  where
    show Point { getPoint (x,y) } = "(" ++ show x ++ "," ++ show y ++ ")"

  mk :: Int -> Int -> Point
  mk x y = Point { getPoint (x,y) }

  diagonal :: Point -> Bool
  diagonal Point { getPoint (x,y) } = x == y

  getX :: Point -> Int
  getX Point { getPoint (x,_) } = x

  getY :: Point -> Int
  getY Point { getPoint (x,_) } = x
