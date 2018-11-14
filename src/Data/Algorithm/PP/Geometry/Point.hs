-- module Data.Algorithm.PP.Geometry.Point
-- (
--
-- )
-- where

  import qualified Data.Tuple as T

  newtype Point = Point (Int, Int) deriving (Eq)

  mk :: Int -> Int -> Point
  mk x y = Point (x, y)

  instance Show (Point) where
    show (Point (x, y)) = show (x, y)

  assocX :: Int -> [Int] -> [Point]
  assocX yStart = zipWith (flip mk) [yStart..]

  assocX' :: [Int] -> [Point]
  assocX' = assocX 1

  assocY :: Int -> [Int] -> [Point]
  assocY xStart = zipWith mk [xStart..]

  assocY' :: [Int] -> [Point]
  assocY' = assocY 1

  toPair :: Point -> (Int, Int)
  toPair (Point (x, y)) = (x, y)

  getX :: Point -> Int
  getX (Point (x, _)) = x

  getY :: Point -> Int
  getY (Point (_, y)) = y
