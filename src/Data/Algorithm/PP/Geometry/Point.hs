module Data.Algorithm.PP.Geometry.Point
(
  Point(..)

, mk
, assocX
, assocX'
, assocY
, assocY'

, getX
, getY
)
where

  import qualified Data.Tuple as T

  newtype Point = Point { getCoordinates :: (Int, Int) } deriving (Eq)

  instance Show Point where
    show = show . getCoordinates

  mk :: Int -> Int -> Point
  mk x y = Point { getCoordinates = (x, y) }

  assocX :: Int -> [Int] -> [Point]
  assocX yStart = zipWith (flip mk) [yStart..]

  assocX' :: [Int] -> [Point]
  assocX' = assocX 1

  assocY :: Int -> [Int] -> [Point]
  assocY xStart = zipWith mk [xStart..]

  assocY' :: [Int] -> [Point]
  assocY' = assocY 1

  getX :: Point -> Int
  getX = T.fst . getCoordinates

  getY :: Point -> Int
  getY = T.snd . getCoordinates
