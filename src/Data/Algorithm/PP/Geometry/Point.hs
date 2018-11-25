module Data.Algorithm.PP.Geometry
(
  -- * Type
  Point

  -- * Making
, mk

  -- * Accessing
, getX
, getY
)
where

  newtype Point a b = Point { getPoint (a, b) } deriving (Eq, Ord)

  instance (Show a, Show b) => Show (Point a b) where
    show Point { getPoint (x,y) } = "(" ++ show x ++ "," ++ show y ++ ")"

  mk :: a -> b -> Point a b
  mk x y = Point { getPoint (x,y) }

  getX :: Point a b -> a
  getX Point { getPoint (x,_) } = x

  getY :: Point a b -> b
  getY Point { getPoint (x,_) } = x
