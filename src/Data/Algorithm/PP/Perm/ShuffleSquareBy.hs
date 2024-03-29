module Data.Algorithm.PP.Perm.ShuffleSquareBy
  (
    -- * Searching
    shuffleSquareRootsBy
  , shuffleSquareRootByCount

    -- * Testing
  , shuffleSquareBy
  , simpleShuffleSquareBy
  , kShuffleSquareBy
  , extremalShuffleSquareBy
  , kShuffleSquareByFree
  , shuffleSquareByFree

    -- * Generating
  , shuffleSquaresBy
  , nonShuffleSquaresBy

    -- * Sub-permutations
  , subShuffleSquaresBy
  ) where

import qualified Control.Arrow  as A
import qualified Data.Foldable  as F
import qualified Data.List      as L
import qualified Data.Tuple     as T

import qualified Data.Algorithm.PP.Perm           as PP.Perm
import qualified Data.Algorithm.PP.Perm.Generator as PP.Perm.Generator
import qualified Data.Algorithm.PP.Perm.Pattern   as PP.Perm.Pattern
import qualified Data.Algorithm.PP.Utils.Foldable as PP.Utils.Foldable
import qualified Data.Algorithm.PP.Utils.List     as PP.Utils.List

{- | 'shuffleSquareRootsBy' @f@ @p@ returns all square roots of the permutation @p@ according to the function @f@.

\[
\forall p \in S_{n}, \;
\forall f : S_{n} \to S_{n},
\quad
\texttt{ shuffleSquareRootsBy } f \; p
\;=\;
\left\{q : p \in q \bullet f \; q\right\}
\]
-}
shuffleSquareRootsBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> [PP.Perm.Perm]
shuffleSquareRootsBy f = PP.Utils.List.uniq . L.map proj1 . L.filter test . L.map trans . PP.Utils.List.balPartitions . PP.Perm.getList
  where
    trans = PP.Perm.mk A.*** (f . PP.Perm.mk)
    test  = T.uncurry (==)
    proj1 = T.fst

{- | 'shuffleSquareRootByCount' @f@ @p@ returns the number of distinct square roots of the permutation @p@
according to the function @f@.

\[
\forall n \in \mathbb{N}, \;
\forall f : S_{n} \to S_{n},
\quad
\texttt{ shuffleSquareRootsByStat } f \; p
\;=\;
\left|\left\{q : p \in q \bullet f \; q\right\}\right|
\]
-}
shuffleSquareRootByCount :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Int
shuffleSquareRootByCount f = L.length . shuffleSquareRootsBy f

{- | 'shuffleSquareBy' @f@ @p@ returns @True@ if the permutation @p@ is a shuffle-square according to the function @f@. -}
shuffleSquareBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Bool
shuffleSquareBy f = not . L.null . shuffleSquareRootsBy f

{- | 'simpleShuffleSquareBy' @f@ @p@ returns @True@ if the permutations @p@ has exactly one square root according
to the funtion @f@.

>>> simpleShuffleSquareBy comp $ mk [7,3,5,1,2,6,4,8]
True
>>> shuffleSquareRootsBy comp $ mk [7,3,5,1,2,6,4,8] -- check
[[4,2,3,1]]
>>> simpleShuffleSquareBy comp $ mk [7,1,3,5,4,6,2,8]
False
>>> shuffleSquareRootsBy comp $ mk [7,1,3,5,4,6,2,8] -- check
[[3,1,2,4],[4,2,3,1],[4,3,2,1]]
-}
simpleShuffleSquareBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Bool
simpleShuffleSquareBy f = go . shuffleSquareRootsBy f
  where
    go [p] = True
    go _   = False

{- | 'kShuffleSquareBy' @f@ @k@ @p@ returns @True@ if the permutations @p@ has exactly @k@ distinct square roots
according to the function @f@.

>>> kShuffleSquareBy comp 3 $ mk [7,1,3,5,4,6,2,8]
>>> True
>>> shuffleSquareRootsBy comp $ mk [7,1,3,5,4,6,2,8] -- check
[[3,1,2,4],[4,2,3,1],[4,3,2,1]]
>>>  kShuffleSquareBy comp 3 $ mk [7,3,5,1,2,6,4,8]
False
>>> shuffleSquareRootsBy comp $ mk [7,3,5,1,2,6,4,8] -- check
[[4,2,3,1]]
-}
kShuffleSquareBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> Int -> PP.Perm.Perm -> Bool
kShuffleSquareBy f k = (==) k . L.length . shuffleSquareRootsBy f

extremalShuffleSquareBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Bool
extremalShuffleSquareBy f p = False

{- | 'kShuffleSquareByFree' @f@ @p@ returns @True@ if the permutation @p@ does not contain a sub-permutation
@q@ of length @k@ that is a square according to the function @f@.

>>>
-}
kShuffleSquareByFree :: (PP.Perm.Perm -> PP.Perm.Perm) -> Int -> PP.Perm.Perm -> Bool
kShuffleSquareByFree f k p
  | odd k     = True
  | otherwise = F.all (not . shuffleSquareBy f) $ PP.Perm.Pattern.kPatterns k p

-- |'shuffleSquareByFree' 'f' 'p' retusn 'True' if the permutations 'p' does not
-- contain any pattern of length at least 4 that is a shuffle square according
-- to the bijection 'f'.
shuffleSquareByFree :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Bool
shuffleSquareByFree f p = F.and [kShuffleSquareByFree f k p | k <- [4,6..n]]
  where
    n = PP.Perm.len p

-- |'shuffleSquaresBy' 'n' returns all shuffle-square permutations of length 'n'.
shuffleSquaresBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> Int -> [PP.Perm.Perm]
shuffleSquaresBy f n
  | odd n     = []
  | otherwise = L.filter (shuffleSquareBy f) $ PP.Perm.Generator.perms n

-- |'nonShuffleSquaresBy' 'n' returns all non-shuffle-square permutations of length 'n'.
nonShuffleSquaresBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> Int -> [PP.Perm.Perm]
nonShuffleSquaresBy f n
  | odd n     = PP.Perm.Generator.perms n
  | otherwise = L.filter (not . shuffleSquareBy f) $ PP.Perm.Generator.perms n

{- | 'subShuffleSquaresBy' @f@ @p@ returns the longest sub-permutations of the permutation @p@
that is a square according to the function @f@.

>>>
-}
subShuffleSquaresBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> [PP.Perm.Perm]
subShuffleSquaresBy f p = aux $ PP.Perm.len p
  where
    aux k
      | odd k     = aux (k-1)
      | otherwise = if L.null sqs then aux (k-2) else sqs
      where
        sqs  = L.filter (shuffleSquareBy f) $ PP.Perm.Pattern.kPatterns k p
