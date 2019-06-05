{-|
Module      : Data.Algorithm.PP.Perm.Generator.Basic
Description : Generating basic permutations
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Generating basic permutations facilities.
-}

module Data.Algorithm.PP.Perm.Generator.Basic (
    perms
  , lexPerms

  , derangements
  , fixedPoints
  , localizedFixedPoints
  , strictLocalizedFixedPoints

  , extendLeft
  , extendRight
  ) where

import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T

import qualified Data.Algorithm.PP.Perm                   as PP.Perm
import qualified Data.Algorithm.PP.Perm.Bijection.Trivial as PP.Perm.Bijection.Trivial
import qualified Data.Algorithm.PP.Perm.Features          as PP.Perm.Features
import qualified Data.Algorithm.PP.Perm.Property          as PP.Perm.Property

{- | 'perms' @n@ returns all permutations of length @n@.

>>> perms 0
[[]]
>>> perms 1
[[1]]
>>> perms 2
[[1,2],[2,1]]
>>> perms 3
[[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]
-}
perms :: Int -> [PP.Perm.Perm]
perms n = L.map PP.Perm.mk $ L.permutations [1 .. n]

{- | 'lexPerms' @n@ returns all permutations of length @n@ in lexicographic order.
-}
lexPerms :: Int ->[PP.Perm.Perm]
lexPerms _ = []

{- | 'derangements' @n@ returns all derangements of length @n@.

>>> derangements 2
[[2,1]]
>>> derangements 3
[[2,3,1],[3,1,2]]
>>> derangements 4
[[4,3,2,1],[3,4,2,1],[2,3,4,1],[4,1,2,3],[2,4,1,3],[2,1,4,3],[4,3,1,2],[3,4,1,2],[3,1,4,2]]
-}
derangements :: Int -> [PP.Perm.Perm]
derangements = L.filter PP.Perm.Property.derangement . perms

{- | 'fixedPoints' @k@ @n@ returns all permutations of length @n@ with @k@
fixed points.

prop> fixedPoints 0 n = derangements n

>>> fixedPoints 0 4
[[4,3,2,1],[3,4,2,1],[2,3,4,1],[4,1,2,3],[2,4,1,3],[2,1,4,3],[4,3,1,2],[3,4,1,2],[3,1,4,2]]
>>> fixedPoints 1 4
[[2,3,1,4],[3,1,2,4],[3,2,4,1],[2,4,3,1],[1,4,2,3],[4,2,1,3],[4,1,3,2],[1,3,4,2]]
>>> fixedPoints 2 4
[[2,1,3,4],[3,2,1,4],[1,3,2,4],[4,2,3,1],[1,2,4,3],[1,4,3,2]]
>>> fixedPoints 3 4
[]
>>> fixedPoints 4 4
[[1,2,3,4]]
-}
fixedPoints :: Int -> Int -> [PP.Perm.Perm]
fixedPoints k n
  | k > n     = []
  | otherwise = L.filter (\ p -> L.length (PP.Perm.Features.fixedPoints p) == k) $ perms n

-- localizedFixedPoints and strictLocalizedFixedPoints helper function.
localizedFixedPoints' xs n
  | L.null xs       = []
  | L.length xs > n = []
  | minimum xs < 1  = []
  | maximum xs > n  = []
  | otherwise       = L.map (L.sortOn T.fst . (++) pxs . L.zip ys') $ L.permutations ys
    where
      xs' = L.sort xs
      pxs = L.zip xs' xs'

      ys  = [1..n] L.\\ xs
      ys' = L.sort ys

{- | 'localizedFixedPoints' @xs@ @n@ returns all permutations of length @n@ in which elements of @xs@ are fixed points.

>>> localizedFixedPoints [1] 4
[[1,2,3,4],[1,3,2,4],[1,4,3,2],[1,3,4,2],[1,4,2,3],[1,2,4,3]]
>>> localizedFixedPoints [2] 4
[[1,2,3,4],[3,2,1,4],[4,2,3,1],[3,2,4,1],[4,2,1,3],[1,2,4,3]]
>>> localizedFixedPoints [3] 4
[[1,2,3,4],[2,1,3,4],[4,2,3,1],[2,4,3,1],[4,1,3,2],[1,4,3,2]]
>>> localizedFixedPoints [4] 4
[[1,2,3,4],[2,1,3,4],[3,2,1,4],[2,3,1,4],[3,1,2,4],[1,3,2,4]]
>>> localizedFixedPoints [1,2] 4
[[1,2,3,4],[1,2,4,3]]
>>> localizedFixedPoints [1,3] 4
[[1,2,3,4],[1,4,3,2]]
>>> localizedFixedPoints [1,4] 4
[[1,2,3,4],[1,3,2,4]]
>>> localizedFixedPoints [2,3] 4
[[1,2,3,4],[4,2,3,1]]
>>> localizedFixedPoints [2,4] 4
[[1,2,3,4],[3,2,1,4]]
>>> localizedFixedPoints [3,4] 4
[[1,2,3,4],[2,1,3,4]]
>>> localizedFixedPoints [1,2,3] 4
[[1,2,3,4]]
>>> localizedFixedPoints [1,2,4] 4
[[1,2,3,4]]
>>> localizedFixedPoints [1,3,4] 4
[[1,2,3,4]]
>>> localizedFixedPoints [2,3,4] 4
[[1,2,3,4]]
-}
localizedFixedPoints xs = L.map (PP.Perm.mk . L.map T.snd) . localizedFixedPoints' xs

{- | 'localizedFixedPoints' @xs@ @n@ returns all permutations of length @n@ in which only elements of @xs@ are fixed points.

>>> strictLocalizedFixedPoints [1] 4
[[1,3,4,2],[1,4,2,3]]
>>> strictLocalizedFixedPoints [2] 4
[[3,2,4,1],[4,2,1,3]]
>>> strictLocalizedFixedPoints [3] 4
[[2,4,3,1],[4,1,3,2]]
>>> strictLocalizedFixedPoints [4] 4
[[2,3,1,4],[3,1,2,4]]
>>> strictLocalizedFixedPoints [1,2] 4
[[1,2,4,3]]
>>> strictLocalizedFixedPoints [1,3] 4
[[1,4,3,2]]
>>> strictLocalizedFixedPoints [1,4] 4
[[1,3,2,4]]
-}
strictLocalizedFixedPoints xs = L.map (PP.Perm.mk . L.map T.snd) . L.filter f . localizedFixedPoints' xs
  where
    f = F.all (\ (i, j) -> i `L.elem` xs || i /= j)

-- transpose :: Int -> Int -> Perm -> Perm
-- transpose i j = mk $ L.prefix (i-1) xs ++ [xs L.!! i] ++

{- | 'extendLeft' @p@ takes a permutation @p@ of length @n@ and returns all
permutations of length @n+1@ which suffix of length @n@ is order-isomorphic
to @p@.

>>> extendLeft $ mkPerm []
[[1]]
>>> extendLeft $ mkPerm [1]
[[1,2],[2,1]]
>>> extendLeft $ mkPerm [1,2]
[[1,2,3],[2,1,3],[3,1,2]]
>>> extendLeft $ mkPerm [2,1]
[[1,3,2],[2,3,1],[3,2,1]]
-}
extendLeft :: PP.Perm.Perm -> [PP.Perm.Perm]
extendLeft p = L.map PP.Perm.mk [k : f k ys | k <- [1..PP.Perm.len p+1]]
  where
    ys  = PP.Perm.getList p
    f k = F.foldr (\ y acc -> (if y < k then y else y+1) : acc) []

{- | 'extendRight' @p@ takes a permutation @p@ of length @n@ and returns all
permutations of length @n+1@ which prefix of length @n@ is order-isomorphic
to @p@.

>>> extendRight $ mkPerm []
[[1]]
>>> extendRight $ mkPerm [1]
[[2,1],[1,2]]
>>> extendRight $ mkPerm [1,2]
[[2,3,1],[1,3,2],[1,2,3]]
>>> extendRight $ mkPerm [2,1]
[[3,2,1],[3,1,2],[2,1,3]]
-}
extendRight :: PP.Perm.Perm -> [PP.Perm.Perm]
extendRight = L.map PP.Perm.Bijection.Trivial.rev . extendLeft . PP.Perm.Bijection.Trivial.rev
