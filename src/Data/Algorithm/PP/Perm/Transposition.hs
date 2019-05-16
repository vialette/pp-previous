{-|
Module      : Data.Algorithm.PP.Perm.Transposition
Description : Organization numbers
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

-}

module Data.Algorithm.PP.Perm.Transposition (
    transpose
  , safeTranspose
  , transpositions
  , adjacentTranspositions
  ) where

import qualified Control.Arrow as A
import qualified Data.Foldable as F
import qualified Data.List     as L

import qualified Data.Algorithm.PP.Perm.Generator.Basic as PP.Perm.Generator.Basic
import qualified Data.Algorithm.PP.Perm                 as PP.Perm
import qualified Data.Algorithm.PP.Utils.List           as PP.Utils.List
import qualified Data.Algorithm.PP.Utils.Maybe          as PP.Utils.Maybe


{-| 'transpose' @i@ @j@ @p@ returns the transposition @(i,j)@ of permutation @p@
(indices start at 0).

>>> let p = mk [1..4] in mapM_ print [(i, j, transpose i j p) | i <- [0..len p-2], j <- [i+1..len p-1]]
(0,1,[2,1,3,4])
(0,2,[3,2,1,4])
(0,3,[4,2,3,1])
(1,2,[1,3,2,4])
(1,3,[1,4,3,2])
(2,3,[1,2,4,3])
-}
transpose :: Int -> Int -> PP.Perm.Perm -> PP.Perm.Perm
transpose i j = PP.Perm.mk . PP.Utils.List.swapElementsAt i j . PP.Perm.getList

safeTranspose :: Int -> Int -> PP.Perm.Perm -> Maybe PP.Perm.Perm
safeTranspose i j p = PP.Utils.Maybe.whenMaybe checkIJ $ transpose i j p
  where
    n       = PP.Perm.len p
    checkI  = i >= 0 || i < n
    checkJ  = i >= 0 || i < n
    checkIJ = checkI && checkJ

{-| 'transpositions' @p@ returns all transpositions of permutation @p@.

>>> let p = mk [1..4] in transpositions p
[[2,1,3,4],[3,2,1,4],[4,2,3,1],[1,3,2,4],[1,4,3,2],[1,2,4,3]]
-}
transpositions :: PP.Perm.Perm -> [PP.Perm.Perm]
transpositions p = [transpose i j p | i <- [0..n-2], j <- [i+1..n-1]]
  where
    n = PP.Perm.len p

{-| adjacentTranspositions @p@ returns all adjacent transpositions of permutation @p@.

>>> let p = mk [1..4] in adjacentTranspositions p
[[2,1,3,4],[1,3,2,4],[1,2,4,3]]
-}
adjacentTranspositions :: PP.Perm.Perm -> [PP.Perm.Perm]
adjacentTranspositions p = [transpose i (i+1) p | i <- [0..n-2]]
  where
    n = PP.Perm.len p