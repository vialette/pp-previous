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
  import qualified Data.Algorithm.PP.Perm           as PP.Perm

  type Row = [Int]

  data StdYoungTableau = StdYoungTableau { getRows :: [Row] } deriving (Show)

  -- |'empty'
  empty :: StdYoungTableau
  empty = StdYoungTableau []

  -- |'shapeR' 't'
  shapeR :: StdYoungTableau -> [Int]
  shapeR = L.map L.length . getRows

  -- |'shapeC' 't'
  shapeC :: StdYoungTableau -> [Int]
  shapeC = L.map L.length . L.transpose . getRows

  -- |'size' 't'
  size :: StdYoungTableau -> Int
  size = sum . shapeR

  --
  replace :: [Int] -> Int -> ([Int], Int);
  replace [] _ = error "no element greater than y in the list"
  replace (x : xs) e
    | e > x     = (x : xs', e')
    | otherwise = (e : xs, x)
      where
         (xs', e') = replace xs e

  --
  insert :: [[Int]] -> [[Int]] -> Int -> Int -> ([[Int]],[[Int]]);
  insert [] [] e i = ([[e]], [[i]])
  insert (xs : xss) (ys : yss) e i
    | e > L.last xs = ((xs  ++ [e]) : xss, (ys ++ [i]) : yss)
    | otherwise     = (xs' : xss', ys : yss')
      where
        (xs', e')    = replace xs e
        (xss', yss') = insert xss yss e' i

  -- |'robinsonSchensted' 'p'
  robinsonSchensted :: PP.Perm.Perm -> (StdYoungTableau, StdYoungTableau)
  robinsonSchensted = (StdYoungTableau A.*** StdYoungTableau) . aux [] [] 1 . PP.Perm.getList
    where
      aux xs ys _ []      = (xs, ys)
      aux [] [] i (e : es) = aux [[e]] [[i]] (i+1) es
      aux xs ys i (e : es) = aux xs' ys' (i+1) es
        where
          (xs', ys') = insert xs ys e i

  -- |'draw' 't'
  --
  -- >>> (t1, t2) = robinsonSchensted (mkPerm [5,8,1,6,7,4,9,2,3])
  -- >>> putStr $ draw t1
  -- +---+---+---+---+
  -- | 1 | 2 | 3 | 9 |
  -- +---+---+---+---+
  -- | 4 | 6 | 7 |
  -- +---+---+---+
  -- | 5 |
  -- +---+
  -- | 8 |
  -- +---+
  -- >>> putStr $ draw t2
  -- +---+---+---+---+
  -- | 1 | 2 | 5 | 7 |
  -- +---+---+---+---+
  -- | 3 | 4 | 9 |
  -- +---+---+---+
  -- | 6 |
  -- +---+
  -- | 8 |
  -- +---+
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
