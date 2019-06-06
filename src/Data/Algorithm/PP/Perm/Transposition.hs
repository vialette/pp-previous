{-|
Module      : Data.Algorithm.PP.Perm.Transposition
Description : Transpositions
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


{-| 'transpose' @i@ @j@ @p@ returns the permutation obtained from permutation @p@ by swapping
the elements at positions @i@ and @j@ (indices start at 0). The function raises an exception
in case of non-valid indices.

>>> let p = mk [1..4] in mapM_ print [(i, j, transpose i j p) | i <- [0..len p-2], j <- [i+1..len p-1]]
(0,1,[2,1,3,4])
(0,2,[3,2,1,4])
(0,3,[4,2,3,1])
(1,2,[1,3,2,4])
(1,3,[1,4,3,2])
(2,3,[1,2,4,3])
>>> let p = mk [1..4] in transpose (-1) 2 p
*** Exception: Prelude.!!: negative index
>>> let p = mk [1..4] in transpose 0 4 p
*** Exception: Prelude.!!: index too large
-}
transpose :: Int -> Int -> PP.Perm.Perm -> PP.Perm.Perm
transpose i j = PP.Perm.mk . PP.Utils.List.swapElementsAt (i-1) (j-1) . PP.Perm.getList

{-| 'safeTranspose' @i@ @j@ @p@ returns @Nothing@ for out of bounds indices @i@ and @j@,
 and @Just $ transpose i j p@ otherwise.

>>> let p = mk [1..4] in mapM_ print [(i, j, safeTranspose i j p) | i <- [0..len p], j <- [i+1..len p]]
(0,1,Just [2,1,3,4])
(0,2,Just [3,2,1,4])
(0,3,Just [4,2,3,1])
(0,4,Nothing)
(1,2,Just [1,3,2,4])
(1,3,Just [1,4,3,2])
(1,4,Nothing)
(2,3,Just [1,2,4,3])
(2,4,Nothing)
(3,4,Nothing)
-}
safeTranspose :: Int -> Int -> PP.Perm.Perm -> Maybe PP.Perm.Perm
safeTranspose i j p = PP.Utils.Maybe.whenMaybe checkIJ $ transpose i j p
  where
    n       = PP.Perm.len p
    checkI  = i >= 1 && i <= n
    checkJ  = j >= 1 && j <= n
    checkIJ = checkI && checkJ

{-| 'transpositions' @p@ returns all transpositions of permutation @p@.

>>> let p = mk [1..4] in transpositions p
[[2,1,3,4],[3,2,1,4],[4,2,3,1],[1,3,2,4],[1,4,3,2],[1,2,4,3]]
-}
transpositions :: PP.Perm.Perm -> [PP.Perm.Perm]
transpositions p = [transpose i j p | i <- [1 .. n-1], j <- [i+1 .. n]]
  where
    n = PP.Perm.len p

{-| adjacentTranspositions @p@ returns all adjacent transpositions of permutation @p@.

>>> let p = mk [1..4] in adjacentTranspositions p
[[2,1,3,4],[1,3,2,4],[1,2,4,3]]
-}
adjacentTranspositions :: PP.Perm.Perm -> [PP.Perm.Perm]
adjacentTranspositions p = [transpose i (i+1) p | i <- [1 .. n-1]]
  where
    n = PP.Perm.len p
