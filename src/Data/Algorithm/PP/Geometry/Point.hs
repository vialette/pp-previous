-- module Data.Algorithm.PP.Geometry.Point
-- (
--
-- )
-- where

  import qualified Data.Tuple as T

  newtype Point a = Point (a, a) deriving (Eq)

  mk :: a -> a -> Point a
  mk x y = Point (x, y)

  instance (Show a) => Show (Point a) where
    show (Point (x, y)) = show (x, y)

  --assocX :: (Enum a) => a -> [a] -> [Point a]
  assocX yStart = zipWith (curry Point . T.swap) [yStart..]

  assocY :: (Enum a) => a -> [a] -> [Point a]
  assocY xStart = zipWith (curry Point) [xStart..]

  toPair :: Point a -> (a, a)
  toPair (Point (x, y)) = (x, y)

  getX :: Point a -> a
  getX (Point (x, _)) = x

  getY :: Point a -> a
  getY (Point (_, y)) = y
