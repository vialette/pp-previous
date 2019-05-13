{-|
Module      : Data.Algorithm.PP.Perm.Factor
Description : Factors of permutations
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Enumerating factors in permutations.
-}

module Data.Algorithm.PP.Perm.Factor (
  -- * Prefixes
    prefix
  , prefixes
  , evenPrefixes
  , oddPrefixes

   -- * Suffixes
  , suffix
  , suffixes
  , evenSuffixes
  , oddSuffixes

   -- * Factors
  , kFactors
  , factors
  , evenFactors
  , oddFactors
  ) where

import qualified Data.Algorithm.PP.Geometry.Point     as PP.Geometry.Point
import qualified Data.Algorithm.PP.Perm               as PP.Perm
import qualified Data.Algorithm.PP.Perm.Factor.Points as PP.Perm.Factor.Points
import qualified Data.Algorithm.PP.Utils.List         as PP.Utils.List

{- | 'prefix' @k@ @p@ returns the prefix of length @k@ (as a permutation) of permutation @p@.

>>> let p = mk [4,2,6,1,5,3] in [prefix i p | i <- [0..len p + 1]]
[[],[1],[2,1],[2,1,3],[3,2,4,1],[3,2,5,1,4],[4,2,6,1,5,3],[4,2,6,1,5,3]]
-}
prefix :: Int -> PP.Perm.Perm -> PP.Perm.Perm
prefix k = PP.Perm.mk . fmap PP.Geometry.Point.getY . PP.Perm.Factor.Points.prefix k

{- | 'perPrefixes' @p@ returns all prefixes (as permutations) of permutation @p@ as permutations.

>>> prefixes $ mk [4,2,6,1,5,3]
[[1],[2,1],[2,1,3],[3,2,4,1],[3,2,5,1,4],[4,2,6,1,5,3]]
-}
prefixes :: PP.Perm.Perm -> [PP.Perm.Perm]
prefixes = fmap (PP.Perm.mk . fmap PP.Geometry.Point.getY) . PP.Perm.Factor.Points.prefixes

{- | 'evenPrefixes' @p@ returns all even length prefixes (as permutations) of permutation @p@ as permutations.

>>> evenPrefixes $ mk [4,2,6,1,5,3]
[[2,1],[3,2,4,1],[4,2,6,1,5,3]]
-}
evenPrefixes :: PP.Perm.Perm -> [PP.Perm.Perm]
evenPrefixes = fmap (PP.Perm.mk . fmap PP.Geometry.Point.getY) . PP.Perm.Factor.Points.evenPrefixes

{- | 'oddPrefixes' @p@ returns all odd length prefixes of permutation @p@ as permutations.

>>> oddPrefixes $ mk [4,2,6,1,5,3]
[[1],[2,1,3],[3,2,5,1,4]]
-}
oddPrefixes :: PP.Perm.Perm -> [PP.Perm.Perm]
oddPrefixes = fmap (PP.Perm.mk . fmap PP.Geometry.Point.getY) . PP.Perm.Factor.Points.oddPrefixes

{- | 'suffix' @k@ @p@ returns the suffix of length @k@ (as a permutation) of permutation @p@.

>>> let p = mk [4,2,6,1,5,3] in [suffix i p | i <- [0..len p + 1]]
[[],[1],[2,1],[1,3,2],[4,1,3,2],[2,5,1,4,3],[4,2,6,1,5,3],[4,2,6,1,5,3]]
-}
suffix :: Int -> PP.Perm.Perm -> PP.Perm.Perm
suffix k = PP.Perm.mk . fmap PP.Geometry.Point.getY . PP.Perm.Factor.Points.suffix k

{- | 'suffixes' @p@ returns the suffixes (as permutations) of permutation @p@.

>>> suffixes $ mk [4,2,6,1,5,3]
[[1],[2,1],[2,1,3],[3,2,4,1],[3,2,5,1,4],[4,2,6,1,5,3]]
-}
suffixes :: PP.Perm.Perm -> [PP.Perm.Perm]
suffixes = fmap (PP.Perm.mk . fmap PP.Geometry.Point.getY) . PP.Perm.Factor.Points.prefixes

{- | 'evenPrefixes' @p@ returns all even length suffixes (as permutations) of permutation @p@.
-}
evenSuffixes :: PP.Perm.Perm -> [PP.Perm.Perm]
evenSuffixes = fmap (PP.Perm.mk . fmap PP.Geometry.Point.getY) . PP.Perm.Factor.Points.evenSuffixes

{- | 'oddPrefixes' @p@ returns all odd length suffixes (as permutations) of permutations @p@.
-}
oddSuffixes :: PP.Perm.Perm -> [PP.Perm.Perm]
oddSuffixes = fmap (PP.Perm.mk . fmap PP.Geometry.Point.getY) . PP.Perm.Factor.Points.evenSuffixes

{- | 'kFactors' @p@ returns all factors (as permutations) of length @k@ of permutation @p@.

>>> let p = mk [4,2,6,1,5,3] in mapM_ print [kFactors i p | i <- [0..len p + 1]]
[]
[[1]]
[[1,2],[2,1]]
[[1,3,2],[2,1,3],[2,3,1],[3,1,2]]
[[2,4,1,3],[3,2,4,1],[4,1,3,2]]
[[2,5,1,4,3],[3,2,5,1,4]]
[[4,2,6,1,5,3]]
[]
-}
kFactors :: Int -> PP.Perm.Perm -> [PP.Perm.Perm]
kFactors k = PP.Utils.List.uniq . fmap (PP.Perm.mk . fmap PP.Geometry.Point.getY) . PP.Perm.Factor.Points.kFactors k

{- | 'factors' @p@ returns all factors (as permutations) of permutation @p@.

>>> factors $ mk [4,2,6,1,5,3]
[[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5],[1,2,3,4,5,6]]
-}
factors :: PP.Perm.Perm -> [PP.Perm.Perm]
factors = PP.Utils.List.uniq . fmap PP.Perm.mk .  PP.Perm.Factor.Points.factors

{- | 'evenFactors' @p@ returns all even length factors (as permutations) of permutation @p@.

>>> evenFactors $ mk [4,2,6,1,5,3]
[[1,2],[2,1],[2,4,1,3],[3,2,4,1],[4,1,3,2],[4,2,6,1,5,3]]
-}
evenFactors :: PP.Perm.Perm -> [PP.Perm.Perm]
evenFactors = PP.Utils.List.uniq . fmap (PP.Perm.mk . fmap PP.Geometry.Point.getY) . PP.Perm.Factor.Points.evenFactors

{- | 'oddFactors' @p@ returns all odd length factors (as permutations) of permutation @p@.

>>> oddFactors $ mk [4,2,6,1,5,3]
[[1],[1,3,2],[2,1,3],[2,3,1],[2,5,1,4,3],[3,1,2],[3,2,5,1,4]]
-}
oddFactors :: PP.Perm.Perm -> [PP.Perm.Perm]
oddFactors = PP.Utils.List.uniq . fmap (PP.Perm.mk . fmap PP.Geometry.Point.getY) . PP.Perm.Factor.Points.oddFactors
ts.maxFactors f