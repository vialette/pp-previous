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
  , transpositions
  ) where

import qualified Control.Arrow as A
import qualified Data.Foldable as F
import qualified Data.List     as L

import qualified Data.Algorithm.PP.Perm.Generator.Basic as PP.Perm.Generator.Basic
import qualified Data.Algorithm.PP.Perm                 as PP.Perm
import qualified Data.Algorithm.PP.Utils.List           as PP.Utils.List


{- | 'transpose' @i@ @j@ @p@ returns the transposition @(i,j)@ of permutation @p@.

>>>

-}
transpose :: Int -> Int -> PP.Perm.Perm -> PP.Perm.Perm
transpose i j = PP.Perm.mk . PP.Utils.List.swapElementsAt i j . PP.Perm.getList

{- | 'transpositions' @p@ returns all transpositions of permutation @p@.

-}
transpositions :: PP.Perm.Perm -> [PP.Perm.Perm]
transpositions p = [transpose i j p | i <- [0..n-2], j <- [i+1..n-1]]
  where
    n = PP.Perm.len p