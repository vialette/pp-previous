-- module Data.Algorithm.PP.Geometry.Point
-- (
--
-- )
-- where

  import qualified Data.Tuple as T

  newtype Point = Point { getPoint :: (Int, Int) } deriving (Eq)

  instance Show (Point) where
    show = show . getPoint

  mk :: Int -> Int -> Point
  mk x y = Point { getPoint = (x, y) }

  assocX :: Int -> [Int] -> [Point]
  assocX yStart = zipWith (flip mk) [yStart..]

  assocX' :: [Int] -> [Point]
  assocX' = assocX 1

  assocY :: Int -> [Int] -> [Point]
  assocY xStart = zipWith mk [xStart..]

  assocY' :: [Int] -> [Point]
  assocY' = assocY 1

  getX :: Point -> Int
  getX = T.fst . getPoint

  getY :: Point -> Int
  getY = T.snd . getPoint
