{-|
Module      : Data.Algorithm.PP.Perm.DeterministicPrefixReversal
Description : Deterministic pancake reversal
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Deterministic prefix reversals of permutations.
-}
module Data.Algorithm.PP.Perm.DeterministicPrefixReversal
  (
    deterministicPrefixReversals
  , deterministicPrefixReversalRadius
  , longestDeterministicPrefixReversals

  , lastDeterministicPrefixReversals
  , lastDeterministicPrefixReversalRadius
  , lastLongestDeterministicPrefixReversals

  , follow
  ) where

import Control.Applicative
import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T

import qualified Data.Algorithm.PP.Perm            as PP.Perm
import qualified Data.Algorithm.PP.Geometry.Point  as PP.Geometry.Point
import qualified Data.Algorithm.PP.Perm.Generator  as PP.Perm.Generator
import qualified Data.Algorithm.PP.Perm.Statistics as PP.Perm.Statistics
import qualified Data.Algorithm.PP.Utils.Integer   as PP.Utils.Integer
import qualified Data.Algorithm.PP.Utils.Foldable  as PP.Utils.Foldable
import qualified Data.Algorithm.PP.Utils.List      as PP.Utils.List

-- Helper function. Return True if 1 is at the first position.
firstInPlace :: [Int] -> Bool
firstInPlace []      = True
firstInPlace (x : _) = x == 1

-- Helper function. Return True if n is at the last position.
lastInPlace :: Int -> [Int] -> Bool
lastInPlace _ [] = True
lastInPlace n xs = L.last xs == n

-- Helper function parameterized by the stop predicate f.
deterministicPrefixReversals' :: ([Int] -> Bool) -> PP.Perm.Perm -> [PP.Perm.Perm]
deterministicPrefixReversals' f = fmap PP.Perm.mk . L.reverse . aux [] . PP.Perm.getList
  where
    aux acc [] = acc
    aux acc xs
      | f xs      = xs : acc
      | otherwise = aux (xs : acc) $ PP.Utils.List.prefixReversal (L.head xs) xs

{- | 'deterministicPrefixReversals' @p@ returns the list of permutations @q1, q2, ... qk@
with 'p = q1' and every permutation @qi@ is obtained from the preceding permutation
by reversing the prefix of length @l@, where @l@ is the first element.

>>> deterministicPrefixReversal $ mk [4,2,6,4,3,1]
[4,2,6,5,3,1],[5,6,2,4,3,1],[3,4,2,6,5,1],[2,4,3,6,5,1],[4,2,3,6,5,1],[6,3,2,4,5,1],[1,5,4,2,3,6]]
-}
deterministicPrefixReversals :: PP.Perm.Perm -> [PP.Perm.Perm]
deterministicPrefixReversals = deterministicPrefixReversals' firstInPlace


{- | 'longestdeterministicPrefixReversals' @n@ returns the extremal permutations
of length @n@ together with the length of the path.

>>> longestdeterministicPrefixReversals 4
(4,[[3,1,4,2],[2,4,1,3]])
>>> mapM_ (putStr . (\ s -> s ++ "\n") . show) . map deterministicPrefixReversals . snd $ longestdeterministicPrefixReversals 4
[[3,1,4,2],[4,1,3,2],[2,3,1,4],[3,2,1,4],[1,2,3,4]]
[[2,4,1,3],[4,2,1,3],[3,1,2,4],[2,1,3,4],[1,2,3,4]]
>>> longestdeterministicPrefixReversals 5
(7,[[3,1,4,5,2]])
>>> mapM_ (putStr . (\ s -> s ++ "\n") . show) . map deterministicPrefixReversals . snd $ longestdeterministicPrefixReversals 5
[[3,1,4,5,2],[4,1,3,5,2],[5,3,1,4,2],[2,4,1,3,5],[4,2,1,3,5],[3,1,2,4,5],[2,1,3,4,5],[1,2,3,4,5]]
>>> longestdeterministicPrefixReversals 6
(10,[[4,5,6,2,1,3],[4,1,5,2,6,3],[4,1,6,5,2,3],[3,6,5,1,4,2],[5,6,4,1,3,2]])
>>> mapM_ (putStr . (\ s -> s ++ "\n") . show) . map deterministicPrefixReversals . snd $ longestdeterministicPrefixReversals 6
[[4,5,6,2,1,3],[2,6,5,4,1,3],[6,2,5,4,1,3],[3,1,4,5,2,6],[4,1,3,5,2,6],[5,3,1,4,2,6],[2,4,1,3,5,6],[4,2,1,3,5,6],[3,1,2,4,5,6],[2,1,3,4,5,6],[1,2,3,4,5,6]]
[[4,1,5,2,6,3],[2,5,1,4,6,3],[5,2,1,4,6,3],[6,4,1,2,5,3],[3,5,2,1,4,6],[2,5,3,1,4,6],[5,2,3,1,4,6],[4,1,3,2,5,6],[2,3,1,4,5,6],[3,2,1,4,5,6],[1,2,3,4,5,6]]
[[4,1,6,5,2,3],[5,6,1,4,2,3],[2,4,1,6,5,3],[4,2,1,6,5,3],[6,1,2,4,5,3],[3,5,4,2,1,6],[4,5,3,2,1,6],[2,3,5,4,1,6],[3,2,5,4,1,6],[5,2,3,4,1,6],[1,4,3,2,5,6]]
[[3,6,5,1,4,2],[5,6,3,1,4,2],[4,1,3,6,5,2],[6,3,1,4,5,2],[2,5,4,1,3,6],[5,2,4,1,3,6],[3,1,4,2,5,6],[4,1,3,2,5,6],[2,3,1,4,5,6],[3,2,1,4,5,6],[1,2,3,4,5,6]]
[[5,6,4,1,3,2],[3,1,4,6,5,2],[4,1,3,6,5,2],[6,3,1,4,5,2],[2,5,4,1,3,6],[5,2,4,1,3,6],[3,1,4,2,5,6],[4,1,3,2,5,6],[2,3,1,4,5,6],[3,2,1,4,5,6],[1,2,3,4,5,6]]
-}
longestDeterministicPrefixReversals :: Int -> (Int, [[PP.Perm.Perm]])
longestDeterministicPrefixReversals = PP.Utils.Foldable.maximumsBy L.length . fmap deterministicPrefixReversals . PP.Perm.Generator.derangements

{- |'deterministicPrefixReversalRadius' @n@ return the radius (//i.e.// the length
of a longest path for a permutation of length @n@).

>>> deterministicPrefixReversalRadius 2
1
>>> deterministicPrefixReversalRadius 3
2
>>> deterministicPrefixReversalRadius 4
4
>>> deterministicPrefixReversalRadius 5
7
>>> deterministicPrefixReversalRadius 6
10
-}
deterministicPrefixReversalRadius :: Int -> Int
deterministicPrefixReversalRadius = L.length . L.head . T.snd . longestDeterministicPrefixReversals

{- | 'lastDeterministicPrefixReversals' @p@
-}
lastDeterministicPrefixReversals :: PP.Perm.Perm -> [PP.Perm.Perm]
lastDeterministicPrefixReversals p = deterministicPrefixReversals' (lastInPlace (PP.Perm.len p)) p

{- | 'lastLongestDeterministicPrefixReversals' @n@
-}
lastLongestDeterministicPrefixReversals = PP.Utils.Foldable.maximumsBy L.length . fmap lastDeterministicPrefixReversals . PP.Perm.Generator.derangements

{- | 'lastLongestDeterministicPrefixReversals' @n@
-}
lastLongestDeterministicPrefixReversals :: Int -> (Int, [[PP.Perm.Perm]])
lastDeterministicPrefixReversalRadius = L.length . L.head . T.snd . lastLongestDeterministicPrefixReversals

{- | 'follow' @n@ @p@
-}
follow :: Functor f => Int -> f PP.Perm.Perm -> f (Maybe Int)
follow i = fmap (liftA PP.Geometry.Point.getX . F.find f . PP.Perm.getPoints)
  where
    f = (== i) . PP.Geometry.Point.getY
