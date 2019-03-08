module Data.Algorithm.PP.Utils.List (
    safeHead
  , safeTail
  , safeLast

  , splitOn
  , splitAt
  , splitEvery

  , insertAfterMax

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

  , subsets
  , partitions
  , balPartitions

    -- * Random
  , randomShuffle
  ) where

import System.Random
import Data.Maybe
import Prelude hiding (splitAt)
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

-- |'safeLast' 'xs'
safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (L.last xs)

-- |'splitOn' 'x' 'xs'
splitOn :: (Ord a) => a -> [a] -> ([a], [a])
splitOn x = aux []
  where
    aux acc [] = (L.reverse acc, [])
    aux acc (x' : xs)
      | x' < x    = aux (x' : acc) xs
      | otherwise = (L.reverse acc, xs)

-- |'splitEvery 'p' 'xs' return the pair @(xs', xs'')@, @xs ==  xs' ++ xs''@,
-- where @xs'@ is the shortest prefix of @xs@ such that @p (last xs') == True@.
-- The function returns @(xs, [])@ if @p x == False@ for every element of 'xs'.
splitAt :: (a -> Bool) -> [a] -> ([a], [a])
splitAt p = aux []
  where
    aux acc [] = (L.reverse acc, [])
    aux acc (x : xs)
      | p x       = (L.reverse (x : acc), xs)
      | otherwise = aux (x : acc) xs

{- | 'splitEvery @p@ @xs@
-}
splitEvery :: (a -> Bool) -> [a] -> [[a]]
splitEvery p = aux [] . splitAt p
  where
    aux acc (xs, [])  = L.reverse (xs : acc)
    aux acc (xs, xss) = aux (xs : acc) $ splitAt p xss

insertAfterMax :: (Eq a, Ord a) => a-> [a] -> [a]
insertAfterMax x xs = xs' ++ [y, x] ++ xs''
  where
    (xs', y : xs'') = flip L.splitAt xs . fromJust . flip L.elemIndex xs $ F.maximum xs

-- |'groupBy''
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _   []       = []
groupBy' cmp (x : xs) = (x : ys) : groupBy' cmp zs
    where
      (ys, zs) = spanCmp x xs
        where
          spanCmp _  [] = ([], [])
          spanCmp x' (x'' : xs')
            | x' `cmp` x'' = let (ps, qs) = spanCmp x'' xs' in (x'' : ps, qs)
            | otherwise  = ([], x'' : xs')

{- |'factor' @xs@
-}
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

-- |'reversal' 'i' 'j' 'xs' return the list
--
-- >>>
reversal :: Int -> Int -> [a] -> [a]
reversal i j xs = ps ++ L.reverse ys ++ ss
  where
    (ps, xs') = L.splitAt (i-1) xs
    (ys, ss)  = L.splitAt (j-i+1) xs'

-- |'reversal'' 'i' 'm' 'xs'
--
-- >>>
reversal' :: Int -> Int -> [a] -> [a]
reversal' i m = reversal i (i+m-1)

-- |'prefixReversal' 'm' 'xs'
--
-- >>>
prefixReversal :: Int -> [a] -> [a]
prefixReversal = reversal' 1

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

{- |'subsets' @k@ @xs@ returns all @k@-subsets of @xs@.

>>> subsets 0 [1..4]
[[]]
>>> subsets 1 [1..4]
[[1],[2],[3],[4]]
>>> subsets 2 [1..4]
[[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
>>> subsets 3 [1..4]
[[1,2,3],[1,2,4],[1,3,4],[2,3,4]]
>>> subsets 4 [1..4]
[[1,2,3,4]]
>>> subsets 5 [1..4]
[]
-}
subsets :: (Foldable t, Eq b, Num b) => b -> t a -> [[a]]
subsets k = aux k . F.toList
  where
    aux 0  _        = [[]]
    aux _  []       = []
    aux k' (x : xs) = [x : xs' | xs' <- aux (k'-1) xs] ++ aux k' xs

-- |'partitions' 'nl' 'nr' 'xs'  return all possible partitions of 'xs' into 'ns'
-- and 'nr' elements.
--
-- >>> [(i, j, partitions i j [1..4]) | i <- [0..4], j <- [0..4], i+j == 4]
-- [(0,4,[([],[1,2,3,4])])
-- ,(1,3,[([1],[2,3,4]),([2],[1,3,4]),([3],[1,2,4]),([4],[1,2,3])])
-- ,(2,2,[([1,2],[3,4]),([1,3],[2,4]),([1,4],[2,3]),([2,3],[1,4]),([2,4],[1,3]),([3,4],[1,2])])
-- ,(3,1,[([1,2,3],[4]),([1,2,4],[3]),([1,3,4],[2]),([2,3,4],[1])])
-- ,(4,0,[([1,2,3,4],[])])]
partitions :: (Eq a, Foldable t) => Int -> Int -> t a -> [([a], [a])]
partitions nl nr = aux . F.toList
  where
    aux xs
      | nl + nr /= F.length xs = []
      | otherwise              = [(xs', xs L.\\ xs') | xs' <- subsets nl xs]

-- |'balPartitions' 'xs' returns all partitions of 'xs' into 'n/2' and 'n/2'
-- elements, where 'n' is the length of 'xs'.
--
-- >>> balPartitions [1..4]
-- [([1,2],[3,4]),([1,3],[2,4]),([1,4],[2,3])]
-- >>> balPartitions [1..5]
-- []
balPartitions :: (Foldable t, Ord a) => t a -> [([a], [a])]
balPartitions = aux . F.toList
  where
    aux []       = []
    aux (x : xs) = PP.Utils.List.uniq $ fmap f ps
      where
        f p = (x : T.fst p, T.snd p)
        ps  = partitions (k-1) k xs
        k   = 1 + F.length xs `div` 2