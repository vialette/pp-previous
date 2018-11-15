module Data.Algorithm.PP.Geometry.Point
(
  Point(..)

, mk

, index

, getX
, getY
)
where

  import qualified Data.Tuple as T

  newtype Point a = Point { getCoordinates :: (a, a) } deriving (Eq)

  instance Show Point where
    show = show . getCoordinates

  mk :: a -> a -> Point a
  mk x y = Point { getCoordinates = (x, y) }

  assocX :: a -> [a] -> [Point a]
  assocX yStart = zipWith (flip mk) [yStart..]

  assocX' :: [a] -> [Point a]
  assocX' = assocX 1

  assocY :: a -> [a] -> [Point a]
  assocY xStart = zipWith mk [xStart..]

  assocY' :: [a] -> [Point a]
  assocY' = assocY 1

  getX :: Point a -> a
  getX = T.fst . getCoordinates

  getY :: Point a -> a
  getY = T.snd . getCoordinates
