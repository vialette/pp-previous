module Data.Algorithm.PP.Utils.List
(
  safeHead
, safeTail

, groupBy'

, factor
, factor'

, uniq
, chunk
, chunk2
, chunk3

, shuffle
, perfectShuffle

, reversal
, reversal'
, prefixReversal

  -- * Random
, randomShuffle
)
where

  import System.Random
  import qualified Control.Arrow   as A
  import qualified Data.Foldable   as F
  import qualified Data.List       as L
  import Data.Map.Strict ((!))
  import qualified Data.Map.Strict as M
  import qualified Data.Set        as S

  -- |'safeHead' 'xs'
  safeHead :: [a] -> Maybe a
  safeHead []      = Nothing
  safeHead (x : _) = Just x

  -- |'safeTail' 'xs'
  safeTail :: [a] -> Maybe [a]
  safeTail []       = Nothing
  safeTail (_ : xs) = Just xs

  -- |'groupBy''
  groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
  groupBy' _ [] = []
  groupBy' cmp (x : xs) = (x : ys) : groupBy' cmp zs
      where
        (ys, zs) = spanCmp x xs
          where
            spanCmp _ [] = ([], [])
            spanCmp x' (x'' : xs')
              | x' `cmp` x'' = let (ps, qs) = spanCmp x'' xs' in (x'' : ps, qs)
              | otherwise  = ([], x'' : xs')


  -- |'factor' 'xs'
  factor :: Int -> Int -> [a] -> [a]
  factor i j = L.take (j-i+1) . L.drop i

  -- |'factor'' 'xs'
  factor' :: Int -> Int -> [a] -> [a]
  factor' i k = factor i (i+k-1)

  -- |'uniq' xs removes all duplicates in 'xs'. No order is guaranteed.
  --
  -- >>> uniq "abcacbb"
  -- "abc"
  uniq :: Ord a => [a] -> [a]
  uniq = S.toList . S.fromList

  chunk :: Int -> [a] -> [[a]]
  chunk n = L.takeWhile ((== n) . L.length) . L.transpose . L.take n . L.iterate L.tail

  chunk2 :: [a] -> [(a, a)]
  chunk2 = L.map (\[x, y] -> (x, y)) . chunk 2

  chunk3 :: [a] -> [(a, a, a)]
  chunk3 = L.map (\[x, y, z] -> (x, y, z)) . chunk 3

  shuffle2 :: [a] -> [a] -> [[a]]
  shuffle2 []       []       = [[]]
  shuffle2 []       ys       = [ys]
  shuffle2 xs       []       = [xs]
  shuffle2 (x : xs) (y : ys) = L.map (x :) withY ++ L.map (y :) withX
    where
      withX = shuffle2 (x : xs) ys
      withY = shuffle2 xs       (y : ys)

  shuffle :: [[a]] -> [[a]]
  shuffle []               = [[]]
  shuffle [xs]             = [xs]
  shuffle (xs : xs' : xss) = F.concat [ shuffle2 ys ys' |
                                        ys  <- shuffle2 xs xs'
                                      , ys' <- shuffle xss]

  -- |'perfectShuffle' 'xs' 'ys' perfectly interleaves the lists 'xs' and 'ys',
  -- stating with 'xs'.
  --
  -- >>> perfectShuffle [1,2,3,4] [5,6,7,8]
  -- [1,5,2,6,3,7,4,8]
  -- >>> perfectShuffle [1,2,3,4] [5,6]
  -- [1,5,2,6,3,4]
  -- >>> perfectShuffle [1,2] [5,6,7,8]
  -- [1,5,2,6,7,8]
  perfectShuffle :: [a] -> [a] -> [a]
  perfectShuffle = aux []
    where
      aux acc []       ys = L.reverse acc ++ ys
      aux acc (x : xs) ys = aux (x : acc) ys xs

  -- reversal i j xs returns the elements of xs between position i and i+j
  reversal :: Int -> Int -> [a] -> [a]
  reversal i j xs = ps ++ L.reverse ys ++ ss
    where
      (ps, ss') = L.splitAt i xs
      (ys, ss)  = L.splitAt j ss'

  reversal' :: Int -> Int -> [a] -> [a]
  reversal' i m = reversal i (i+m-1)

  prefixReversal :: Int -> [a] -> [a]
  prefixReversal m = reversal 0 (m-1)

  randomShuffleStep :: RandomGen g => Int -> (M.Map Int a, g) ->  (M.Map Int a, g)
  randomShuffleStep i (m, g) = ((M.insert j (m ! i) . M.insert i (m ! j)) m, g')
    where
      (j, g') = randomR (1, i) g

  randomShuffle :: RandomGen g => [a] -> g -> ([a], g)
  randomShuffle [] g = ([], g)
  randomShuffle xs g = A.first M.elems $ foldr randomShuffleStep initial [1..n]
    where
      n       = L.length xs
      initial = (M.fromList $ L.zip [1..] xs, g)
