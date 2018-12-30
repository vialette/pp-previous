module Data.Algorithm.PP.Utils.Foldable (
    maximumBy
  , maximumBy0
  , maximumBy1

  , subsets
  , partitions
  , balPartitions

  , splitEvery
  ) where

import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T

import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List

-- |'maximumBy' 'f' 'z' 'e' returns
--
-- >>> maximumBy length 0 [] [[1],[2..3],[4..6],[7],[8..9],[10..12]]
-- (3,[[4,5,6],[10,11,12]])
-- >>> maximumBy head 0 [] [[1],[2..3],[4..6],[7],[8..9],[10..12]]
-- (10,[[10,11,12]])
maximumBy :: (Foldable t, Ord b) => (a -> b) -> b -> [a] -> t a -> (b, [a])
maximumBy f z e = F.foldr aux (z, e)
  where
    aux x (m, acc)
      | k > m     = (k, [x])
      | k == m    = (m, x : acc)
      | otherwise = (m, acc)
        where
          k = f x

maximumBy0 :: (Foldable t, Ord b, Num b) => (a -> b) -> t a -> (b, [a])
maximumBy0 f = maximumBy f 0 []

maximumBy1 :: (Foldable t, Ord b, Num b) => (a -> b) -> t a -> (b, [a])
maximumBy1 f = maximumBy f 1 []

-- |Return all subsets
--
-- >>> subsets 0 [1..4]
-- [[]]
-- >>> subsets 1 [1..4]
-- [[1],[2],[3],[4]]
-- >>> subsets 2 [1..4]
-- [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
-- >>> subsets 3 [1..4]
-- [[1,2,3],[1,2,4],[1,3,4],[2,3,4]]
-- >>> subsets 4 [1..4]
-- [[1,2,3,4]]
-- >>> subsets 5 [1..4]
-- []
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

splitEvery :: Foldable t => (a -> Bool) -> t a -> [[a]]
splitEvery p = (\ (xs, xss) -> if L.null xs then xss else xs : xss) . F.foldr f ([], [])
  where
    f x (currentAcc, acc)
      | p x       = if L.null currentAcc then ([x], acc) else ([x], currentAcc : acc)
      | otherwise = (x : currentAcc, acc)
