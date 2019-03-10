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

{- |
-}
prefix :: Int -> PP.Perm.Perm -> PP.Perm.Perm
prefix k = PP.Perm.mk . fmap PP.Geometry.Point.getY . PP.Perm.Factor.Points.prefix k

{- | 'perPrefixes' @p@ returns all prefixes of permutations @p@ as permutations.
-}
prefixes :: PP.Perm.Perm -> [PP.Perm.Perm]
prefixes = fmap (PP.Perm.mk . fmap PP.Geometry.Point.getY) . PP.Perm.Factor.Points.prefixes

{- | 'evenPrefixes' @p@ returns all even length prefixes of permutations @p@ as permutations.
-}
evenPrefixes :: PP.Perm.Perm -> [PP.Perm.Perm]
evenPrefixes = fmap (PP.Perm.mk . fmap PP.Geometry.Point.getY) . PP.Perm.Factor.Points.evenPrefixes

{- | 'oddPrefixes' @p@ returns all odd length prefixes of permutations @p@ as permutations.
-}
oddPrefixes :: PP.Perm.Perm -> [PP.Perm.Perm]
oddPrefixes = fmap (PP.Perm.mk . fmap PP.Geometry.Point.getY) . PP.Perm.Factor.Points.oddPrefixes

{- | 'suffix' @k@ @p@ returns the suffix of length @k@ of permutation @p@ as a permutation.
-}
suffix :: Int -> PP.Perm.Perm -> PP.Perm.Perm
suffix k = PP.Perm.mk . fmap PP.Geometry.Point.getY . PP.Perm.Factor.Points.suffix k


{- | 'suffixes' @p@ returns the suffixes of permutations @p@ as permutations.

-}
suffixes :: PP.Perm.Perm -> [PP.Perm.Perm]
suffixes = fmap (PP.Perm.mk . fmap PP.Geometry.Point.getY) . PP.Perm.Factor.Points.prefixes

{- | 'evenPrefixes' @p@ returns all even length suffixes of permutations @p@ as permutations.
-}
evenSuffixes :: PP.Perm.Perm -> [PP.Perm.Perm]
evenSuffixes = fmap (PP.Perm.mk . fmap PP.Geometry.Point.getY) . PP.Perm.Factor.Points.evenSuffixes

{- | 'oddPrefixes' @p@ returns all odd length suffixes of permutations @p@ as permutations.
-}
oddSuffixes :: PP.Perm.Perm -> [PP.Perm.Perm]
oddSuffixes = fmap (PP.Perm.mk . fmap PP.Geometry.Point.getY) . PP.Perm.Factor.Points.evenSuffixes

{- | 'kFactors' @p@ returns all factors (as permutations) of length @k@ of permutation @p@.
-}
kFactors :: Int -> PP.Perm.Perm -> [PP.Perm.Perm]
kFactors k = fmap (PP.Perm.mk . fmap PP.Geometry.Point.getY) . PP.Perm.Factor.Points.kFactors k

{- | 'factors' @p@ returns all factors (as permutations) of permutation @p@.
-}
factors :: PP.Perm.Perm -> [PP.Perm.Perm]
factors = PP.Utils.List.uniq . fmap PP.Perm.mk .  PP.Perm.Factor.Points.factors

{- | 'evenFactors' @p@ returns all even length factors (as permutations) of permutation @p@.
-}
evenFactors :: PP.Perm.Perm -> [PP.Perm.Perm]
evenFactors = fmap (PP.Perm.mk . fmap PP.Geometry.Point.getY) . PP.Perm.Factor.Points.evenFactors

{- | 'oddFactors' @p@ returns all odd length factors (as permutations) of permutation @p@.
-}
oddFactors :: PP.Perm.Perm -> [PP.Perm.Perm]
oddFactors = fmap (PP.Perm.mk . fmap PP.Geometry.Point.getY) . PP.Perm.Factor.Points.oddFactors

