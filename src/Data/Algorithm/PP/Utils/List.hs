{-|
Module      : Data.Algorithm.PP.Utils.List
Description : Useful functions on lists
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Convenient functions on lists.
-}

module Data.Algorithm.PP.Utils.List
  (
    module PP.Utils.List.Safe

  , swapElementsAt

  , evens
  , odds

  , kDeleteAt
  , deleteAt

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

  -- * Reversal
  , reversal
  , safeReversal
  , reversal'
  , safeReversal'
  , prefixReversal

  , subsets
  , subsets2
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
import qualified Data.Tuple      as T

import Data.Algorithm.PP.Utils.List.Safe as PP.Utils.List.Safe


{- | 'swapElementsAt' @i@ @j@ @xs@
-}
swapElementsAt :: Int -> Int -> [a] -> [a]
swapElementsAt i j xs = left ++ [elemJ] ++ middle ++ [elemI] ++ right
  where
    elemI  = xs L.!! i
    elemJ  = xs L.!! j
    left   = L.take i xs
    middle = L.take (j-i-1) $ L.drop (i+1) xs
    right  = L.drop (j+1) xs

kDeleteAt :: Int -> Int -> [a] -> [a]
kDeleteAt _ _ [] = []
kDeleteAt i k xs = T.uncurry (++) . A.second (L.drop k) $ L.splitAt i xs

deleteAt :: Int -> [a] -> [a]
deleteAt i = kDeleteAt i 1

{- | 'evens' @xs@ returns the elements returns the elements of @xs@ at all even positions.
-}
odds :: [a] -> [a]
odds []       = []
odds (x : xs) = x : evens xs

{- | 'odd' @xs@ returns the elements returns the elements of @xs@ at all odd positions.
-}
evens :: [a] -> [a]
evens []       = []
evens (_ : xs) = odds xs

{- | 'splitOn' @x@ @xs@
-}
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

{- | 'groupBy'' @xs@
-}
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

{- | 'uniq' @xs@ removes all duplicates in @xs@. No order is guaranteed.

>>> uniq "abcacbb"
"abc"
-}
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

{- | 'reversal' @i@ @j@ @xs@ returns the list obtained by reversing the elements at positions @i@,
@i+1@, ..., @j@ in @xs@. Indices start at 1.

>>> let n = 3 in mapM_ print [(i, j, reversal i j [1..n]) | i <- [0..n+1], j <- [i-1..n+1]]
(0,-1,[1,2,3])
(0,0,[1,2,3])
(0,1,[1,2,3])
(0,2,[1,2,3])
(0,3,[1,2,3])
(0,4,[1,2,3])
(1,0,[1,2,3])
(1,1,[1,2,3])
(1,2,[2,1,3])
(1,3,[3,2,1])
(1,4,[1,2,3])
(2,1,[1,2,3])
(2,2,[1,2,3])
(2,3,[1,3,2])
(2,4,[1,2,3])
(3,2,[1,2,3])
(3,3,[1,2,3])
(3,4,[1,2,3])
(4,3,[1,2,3])
(4,4,[1,2,3])
-}
reversal :: Int -> Int -> [a] -> [a]
reversal i j xs = case safeReversal i j xs of
                    Nothing  -> xs
                    Just xs' -> xs'

{- | 'safeReversal' @i@ @j@ @xs@ returns the list obtained by reversing the elements at positions @i@,
@i+1@, ..., @j@ in @xs@. Indices start at 1.

>>> let n = 3 in mapM_ print [(i, j, safeReversal i j [1..n]) | i <- [0..n+1], j <- [i-1..n+1]]
(0,-1,Nothing)
(0,0,Nothing)
(0,1,Nothing)
(0,2,Nothing)
(0,3,Nothing)
(0,4,Nothing)
(1,0,Nothing)
(1,1,Just [1,2,3])
(1,2,Just [2,1,3])
(1,3,Just [3,2,1])
(1,4,Nothing)
(2,1,Nothing)
(2,2,Just [1,2,3])
(2,3,Just [1,3,2])
(2,4,Nothing)
(3,2,Nothing)
(3,3,Just [1,2,3])
(3,4,Nothing)
(4,3,Nothing)
(4,4,Nothing)
-}
safeReversal :: Int -> Int -> [a] -> Maybe [a]
safeReversal i j xs
  | i <= 0 || j <= 0 || j < i || i > n || j > n = Nothing
  | otherwise                                   = Just $ pref ++ L.reverse ys ++ suff
  where
    n = L.length xs
    (pref, xs') = L.splitAt (i-1)   xs
    (ys, suff)  = L.splitAt (j-i+1) xs'


{- | 'reversal'' @i@ @k@ @xs@ returns the list obtained by reversing the elements at positions @i@,
@i+1@, ..., @i+k@ in @xs@. Indices start at 1.

>>> let n = 3 in mapM_ print [(i, k, reversal' i k [1..n]) | i <- [0..n+1], k <- [0..n+1]]
(0,0,[1,2,3])
(0,1,[1,2,3])
(0,2,[1,2,3])
(0,3,[1,2,3])
(0,4,[1,2,3])
(1,0,[1,2,3])
(1,1,[1,2,3])
(1,2,[2,1,3])
(1,3,[3,2,1])
(1,4,[1,2,3])
(2,0,[1,2,3])
(2,1,[1,2,3])
(2,2,[1,3,2])
(2,3,[1,2,3])
(2,4,[1,2,3])
(3,0,[1,2,3])
(3,1,[1,2,3])
(3,2,[1,2,3])
(3,3,[1,2,3])
(3,4,[1,2,3])
(4,0,[1,2,3])
(4,1,[1,2,3])
(4,2,[1,2,3])
(4,3,[1,2,3])
(4,4,[1,2,3])
-}
reversal' :: Int -> Int -> [a] -> [a]
reversal' i k xs = case safeReversal' i k xs of
                     Nothing  -> xs
                     Just xs' -> xs'

{- | 'safeReversal'' @i@ @k@ @xs@ returns the list obtained by reversing the elements at positions @i@,
@i+1@, ..., @i+k@ in @xs@. Indices start at 1.

>>> let n = 3 in mapM_ print [(i, k, safeReversal' i k [1..n]) | i <- [0..n+1], k <- [0..n+1]]
(0,0,Nothing)
(0,1,Nothing)
(0,2,Nothing)
(0,3,Nothing)
(0,4,Nothing)
(1,0,Nothing)
(1,1,Just [1,2,3])
(1,2,Just [2,1,3])
(1,3,Just [3,2,1])
(1,4,Nothing)
(2,0,Nothing)
(2,1,Just [1,2,3])
(2,2,Just [1,3,2])
(2,3,Nothing)
(2,4,Nothing)
(3,0,Nothing)
(3,1,Just [1,2,3])
(3,2,Nothing)
(3,3,Nothing)
(3,4,Nothing)
(4,0,Nothing)
(4,1,Nothing)
(4,2,Nothing)
(4,3,Nothing)
(4,4,Nothing)
-}
safeReversal' :: Int -> Int -> [a] -> Maybe [a]
safeReversal' i k = safeReversal i (i+k-1)

{- |'prefixReversal' @k@ @xs@ returns the list obtained by reversing the prefix of length @k@ of @xs@.

>>> let n = 4 in mapM_ print [(k, prefixReversal k [1..n]) | k <- [1..n]]
(1,[1,2,3,4])
(2,[2,1,3,4])
(3,[3,2,1,4])
(4,[4,3,2,1])
-}
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

{- | 'subsets' @k@ @xs@ returns all @k@-subsets of @xs@.

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
subsets :: (Eq b, Num b) => b -> [a] -> [[a]]
subsets k = aux k . F.toList
  where
    aux 0  _        = [[]]
    aux _  []       = []
    aux k' (x : xs) = [x : xs' | xs' <- aux (k'-1) xs] ++ aux k' xs

subsets2 :: [a] -> [(a, a)]
subsets2 = L.map (\ [x, y] -> (x, y)) . subsets 2

{- | 'partitions' @nl@ @nr@ @xs@  return all possible partitions of @xs@ into @ns@ and @nr@ elements.

>>> [(i, j, partitions i j [1..4]) | i <- [0..4], j <- [0..4], i+j == 4]
[(0,4,[([],[1,2,3,4])])
,(1,3,[([1],[2,3,4]),([2],[1,3,4]),([3],[1,2,4]),([4],[1,2,3])])
,(2,2,[([1,2],[3,4]),([1,3],[2,4]),([1,4],[2,3]),([2,3],[1,4]),([2,4],[1,3]),([3,4],[1,2])])
,(3,1,[([1,2,3],[4]),([1,2,4],[3]),([1,3,4],[2]),([2,3,4],[1])])
,(4,0,[([1,2,3,4],[])])]
-}
partitions :: (Eq a) => Int -> Int -> [a] -> [([a], [a])]
partitions nl nr = aux
  where
    aux xs
      | nl + nr /= F.length xs = []
      | otherwise              = [(xs', xs L.\\ xs') | xs' <- subsets nl xs]

{- | 'balPartitions' @xs@ returns all partitions of @xs@ into @n/2@ and @n/2@, elements,
where @n@ is the length of @xs@.

>>> balPartitions [1..4]
[([1,2],[3,4]),([1,3],[2,4]),([1,4],[2,3])]
>>> balPartitions [1..5]
[]
-}
balPartitions :: (Ord a) => [a] -> [([a], [a])]
balPartitions = aux
  where
    aux []       = []
    aux (x : xs) = uniq $ fmap f ps
      where
        f p = (x : T.fst p, T.snd p)
        ps  = partitions (k-1) k xs
        k   = 1 + F.length xs `div` 2