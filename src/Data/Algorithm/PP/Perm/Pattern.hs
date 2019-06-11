{-|
Module      : Data.Algorithm.PP.Perm.Pattern
Description : Patterns of permutations
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Enumerating patterns in permutations.
-}

module Data.Algorithm.PP.Perm.Pattern (
  -- * Basic patterns
    kPatterns
  , kDistinctPatterns
  , patterns
  , kProlific

  -- * Maximizing
  , maxPatterns

  -- * Parity patterns
  , evenPatterns
  , oddPatterns
  ) where

import qualified Control.Arrow   as Arrow
import qualified Data.Foldable   as F
import Data.Function (on)
import qualified Data.List       as L
import qualified Data.Tuple      as T

import qualified Data.Algorithm.PP.Perm           as PP.Perm
import qualified Data.Algorithm.PP.Perm.Jump      as PP.Perm.Jump
import qualified Data.Algorithm.PP.Utils.Foldable as PP.Utils.Foldable
import qualified Data.Algorithm.PP.Utils.List     as PP.Utils.List

{- | 'kPatterns' @k@ @p@ returns all permutations of length @k@ that occur in permutation @p@.

>>> kPatterns 1 $ mkPerm [1,4,2,5,3]
[[1]]
>>> kPatterns 2 $ mkPerm [1,4,2,5,3]
[[1,2],[2,1]]
>>> kPatterns 3 $ mkPerm [1,4,2,5,3]
[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2]]
>>> kPatterns 4 $ mkPerm [1,4,2,5,3]
[[1,2,4,3],[1,3,2,4],[1,3,4,2],[1,4,2,3],[3,1,4,2]]
>>> kPatterns 5 $ mkPerm [1,4,2,5,3]
[[1,4,2,5,3]]
-}
kPatterns :: Int -> PP.Perm.Perm -> [PP.Perm.Perm]
kPatterns k = L.map PP.Perm.mk . PP.Utils.List.subsets k . PP.Perm.getList

{- | 'kDistinctPatterns' @k@ @p@ returns all distinct permutations of length @k@ that occur in
permutation @p@.

>>>
-}
kDistinctPatterns :: Int -> PP.Perm.Perm -> [PP.Perm.Perm]
kDistinctPatterns k = PP.Utils.List.uniq . kPatterns k

{- | 'patterns' @p@ returns all permutations that occur in permutation @p@.

>>> patterns $ mkPerm [1,4,2,5,3]
[[1],[1,2],[2,1],[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[1,2,4,3],[1,3,2,4],[1,3,4,2],[1,4,2,3],[3,1,4,2],[1,4,2,5,3]]
-}
patterns :: PP.Perm.Perm -> [PP.Perm.Perm]
patterns p = L.concat [kPatterns k p | k <- [1..PP.Perm.len p]]

{- | 'kProlific' @k@ @p@ returns true iff every choice of @k@ deletions yields a different pattern.

Bevan, Homberger, andTenner [2, Theorem 2.22] established a tight connection between the prolific property andminimum Manhattan distance: a permutationπisd-prolific if and only ifd(π)≥d+2
BEVAN, D., HOMBERGER, C.,ANDTENNER, B. E. Prolific permutations and permutedpackings: downsets containing many large patterns.J. Combinatorial Theory, Series A,153 (2018), 98–121.
-}
kProlific :: Int -> PP.Perm.Perm -> Bool
kProlific k p = PP.Perm.Jump.manhattan p >= k+2

{- | 'maxPatterns' @f@ @p@ returns the longest patterns @q@ of permutation @p@ such that @f q@ holds.
-}
maxPatterns :: (PP.Perm.Perm -> Bool) -> PP.Perm.Perm -> [PP.Perm.Perm]
maxPatterns f p = select $ L.dropWhile L.null [[q | q <- kPatterns k p, f q] | k <- [n,n-1..1]]
  where
    n         = PP.Perm.len p
    select ys = if L.null ys then [] else L.head ys

{- | 'evenPatterns' @p@ returns all even length patterns that occur in permutation @p@.

>>> evenPattern $ mkPerm [1,4,2,5,3]
-}
evenPatterns :: PP.Perm.Perm -> [PP.Perm.Perm]
evenPatterns p = L.concat [kPatterns k p | k <- [2,4..n]]
  where
    n = PP.Perm.len p

{- | 'oddPatterns' @p@ returns all even length patterns that occur in permutation @p@.

>>> oddPattern $ mkPerm [1,4,2,5,3]
-}
oddPatterns :: PP.Perm.Perm -> [PP.Perm.Perm]
oddPatterns p = L.concat [kPatterns k p | k <- [1,3..n]]
  where
    n = PP.Perm.len p