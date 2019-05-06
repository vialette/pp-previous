{-|
Module      : Data.Algorithm.PP.Perm.Generator.Alternating
Description : Generating alternating permutations
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Generating alternating permutations facilities.
-}

module Data.Algorithm.PP.Perm.Generator.Alternating (
  -- * Basic
    increasing

  -- * Wedge type 1
  , simpleAlternatingWedgeType1
  , simpleAlternatingWedgeType1s

  -- * Wedge type 2
  , simpleAlternatingWedgeType2
  , simpleAlternatingWedgeType2s
  ) where

import qualified Data.List as L

import qualified Data.Algorithm.PP.Perm            as PP.Perm
import qualified Data.Algorithm.PP.Perm.Combinator as PP.Perm.Combinator
import qualified Data.Algorithm.PP.Utils.List      as PP.Utils.List

{- | 'increasing'

-}
increasing :: Int -> PP.Perm.Perm
increasing n
  | even n    = increasingEven
  | otherwise = increasingEven PP.Perm.Combinator.<<+>> PP.Perm.identity 1
  where
    increasingEven = PP.Perm.Combinator.directSum . L.map PP.Perm.mk $ L.replicate (n `div` 2) [2, 1]


{- | 'simpleAlternatingWedgeType1' @n@ returns the simple alternating wedge type 1 permutation of length @n@.

>>> simpleAlternatingWedgeType1 12
[7,5,8,4,9,3,10,2,11,1,12,6]
-}
simpleAlternatingWedgeType1 :: Int -> PP.Perm.Perm
simpleAlternatingWedgeType1 n = PP.Perm.mk $ PP.Utils.List.perfectShuffle xs ys ++ [kDec+1]
  where
    (kInc, kDec) = if even (n-1) then (n `div` 2, n `div` 2) else (1 + (n `div` 2), n `div` 2)
    xs           = [kDec+2..n]
    ys           = [kDec,kDec-1..1]

{- | 'simpleAlternatingWedgeType1s' return the infinite list of all simple alternating wedge type 1 permutations.

>>> take 8 simpleAlternatingWedgeType1s
[[1],[2,1],[3,1,2],[3,1,4,2],[4,2,5,1,3],[4,2,5,1,6,3],[5,3,6,2,7,1,4],[5,3,6,2,7,1,8,4]]
-}
simpleAlternatingWedgeType1s :: [PP.Perm.Perm]
simpleAlternatingWedgeType1s = [simpleAlternatingWedgeType1 n | n <- [1..]]

simpleAlternatingWedgeType2 :: Int -> PP.Perm.Perm
simpleAlternatingWedgeType2 = PP.Perm.identity

simpleAlternatingWedgeType2s :: [PP.Perm.Perm]
simpleAlternatingWedgeType2s = [simpleAlternatingWedgeType2 n | n <- [1..]]
