module Data.Algorithm.PP.StdYoungTableau
(
  -- * Type
  StdYoungTableau

  -- * Constructing
, empty

  -- * Querying
, shapeR
, shapeC
, size

  -- *
, robinsonSchensted

, draw
)
where

  import qualified Control.Arrow as A
  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Tuple    as T


  import Data.Algorithm.PP.Utils.Int (numDigits)
  import Data.Algorithm.PP.Geometry.Point (X, Y, Point)
  import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point
  import qualified Data.Algorithm.PP.Perm           as PP.Perm

  data StdYoungTableau = StdYoungTableau { getRows :: [[Y]] } deriving (Show)

  empty :: StdYoungTableau
  empty = StdYoungTableau []

  shapeR :: StdYoungTableau -> [Int]
  shapeR = L.map L.length . getRows

  shapeC :: StdYoungTableau -> [Int]
  shapeC = L.map L.length . L.transpose . getRows

  size :: StdYoungTableau -> Int
  size = sum . shapeR

  replace :: [Int] -> Int -> ([Int], Int);
  replace [] _ = error "no element greater than y in the list"
  replace (x : xs) e
    | e > x     = (x : xs', e')
    | otherwise = (e : xs, x)
      where
         (xs', e') = replace xs e

  insert :: [[Int]] -> [[Int]] -> Int -> Int -> ([[Int]],[[Int]]);
  insert [] [] e i = ([[e]], [[i]])
  insert (xs : xss) (ys : yss) e i
    | e > L.last xs = ((xs  ++ [e]) : xss, (ys ++ [i]) : yss)
    | otherwise     = (xs' : xss', ys : yss')
      where
        (xs', e')    = replace xs e
        (xss', yss') = insert xss yss e' i

  robinsonSchensted :: PP.Perm.Perm -> (StdYoungTableau, StdYoungTableau)
  robinsonSchensted = (StdYoungTableau A.*** StdYoungTableau) . aux [] [] 1 . PP.Perm.getList
    where
      aux xs ys _ []      = (xs, ys)
      aux [] [] i (e : es) = aux [[e]] [[i]] (i+1) es
      aux xs ys i (e : es) = aux xs' ys' (i+1) es
        where
          (xs', ys') = insert xs ys e i

  draw :: StdYoungTableau -> String
  draw t = L.intercalate "\n" . aux $ getRows t
    where
      aux [] = [""]
      aux rs = firstLine : F.concat (F.foldr (\ r acc -> row r : acc) [] rs)
        where
          -- inner cell size
          cellSize = numDigits (size t) + 2

          -- stringify the first line
          firstLine = line (L.length $ L.head rs)

          -- stringify a cell
          cell i = replicate lSpaces ' ' ++ show i ++ replicate rSpaces ' '  ++ "|"
            where
              m       = numDigits i
              lSpaces = (cellSize - m) `div` 2
              rSpaces = cellSize - m - lSpaces

          -- stringify a line
          line n = '+' : L.concat (L.replicate n (L.replicate cellSize '-' ++ "+"))

          -- stringify a row
          row xs = [row, sep]
            where
              row = (:) '|' . F.concat $ L.map cell xs
              sep = line (L.length xs)
