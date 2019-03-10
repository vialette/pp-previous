{-|
Module      : Data.Algorithm.PP.Perm.Factor.Points
Description : Factors of permutations
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Factors of permutations as lists of points.
-}

module Data.Algorithm.PP.Perm.Factor.Points
  (
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

  , maxFactors
  ) where

import qualified Data.List as L

import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point
import qualified Data.Algorithm.PP.Perm           as PP.Perm
import qualified Data.Algorithm.PP.Utils.List     as PP.Utils.List

{- | 'prefix' @k@ @p@ returns the prefix (as a list of points) of length @k@ of permutation @p@.

>>> let p = mk [2,4,5,1,6,3] in [prefix i p | i <- [1..len p]]
-}
prefix :: Int -> PP.Perm.Perm -> [PP.Geometry.Point.Point]
prefix k = L.take k . PP.Perm.getPoints

{- | 'prefixes' @p@ returns all prefixes (as lists of points) of permutation @p@.

>>> prefixes $ mk [2,4,5,1,6,3]
-}
prefixes :: PP.Perm.Perm -> [[PP.Geometry.Point.Point]]
prefixes =  L.tail . L.inits . PP.Perm.getPoints

{- | 'evenPrefixes' @p@ returns all even length prefixes (as list of points) of permutations @p@.
-}
evenPrefixes :: PP.Perm.Perm -> [[PP.Geometry.Point.Point]]
evenPrefixes = PP.Utils.List.evens . prefixes

{- | 'oddPrefixes' @p@ returns all odd length prefixes (as lists of points) of permutations @p@.
-}
oddPrefixes :: PP.Perm.Perm -> [[PP.Geometry.Point.Point]]
oddPrefixes = PP.Utils.List.odds . prefixes


{- | 'suffix' @k@ @p@ returns the suffix (as a list of points) of length @k@ of permutation @p@.

>>> let p = mk [2,4,5,1,6,3] in[suffix i p | i <- [1..len p]]
-}
suffix :: Int -> PP.Perm.Perm -> [PP.Geometry.Point.Point]
suffix k p = L.drop (n-k) $ PP.Perm.getPoints p
  where
    n = PP.Perm.len p

{- | 'suffixes' @p@ returns all suffixes (as lists of points) of permutation @p@.

>>> suffixes $ mk [2,4,5,1,6,3]
-}
suffixes :: PP.Perm.Perm -> [[PP.Geometry.Point.Point]]
suffixes = L.init . L.tails . PP.Perm.getPoints


{- | 'evenSuffixes' @p@ returns all even length suffixes (as lists of points) of permutations @p@.
-}
evenSuffixes :: PP.Perm.Perm -> [[PP.Geometry.Point.Point]]
evenSuffixes = PP.Utils.List.evens . suffixes

{- | 'oddSuffixes' @p@ returns all odd length suffixes (as lists of points) of permutations @p@.
-}
oddSuffixes :: PP.Perm.Perm -> [[PP.Geometry.Point.Point]]
oddSuffixes = PP.Utils.List.evens . suffixes

{- | 'kFactors' @k@ @p@ returns all factors (as lists of points) of length @k@ of permutation @p@

>>> kFactors 0 $ mk [2,4,1,3]
[]
>>> kFactors 1 $ mk [2,4,1,3]
[[2],[4],[1],[3]]
>>> kFactors 2 $ mk [2,4,1,3]
[[2,4],[4,1],[1,3]]
>>> kFactors 3 $ mk [2,4,1,3]
[[2,4,1],[4,1,3]]
>>> kFactors 4 $ mk [2,4,1,3]
[[2,4,1,3]]
-}
kFactors :: Int -> PP.Perm.Perm -> [[PP.Geometry.Point.Point]]
kFactors k = PP.Utils.List.chunk k . PP.Perm.getPoints

{- | 'factors' @p@ returns all factors (as lists of points) of permutation @p@.

>>> factors $ mk [2,4,1,3]
-}
factors :: PP.Perm.Perm -> [[PP.Geometry.Point.Point]]
factors p = L.concat [kFactors k p |  k <- [1..PP.Perm.len p]]

{- | 'evenFactors' @p@ returns all even length factors (as lists of points) of permutation @p@. -}
evenFactors :: PP.Perm.Perm -> [[PP.Geometry.Point.Point]]
evenFactors = L.filter (even . L.length) . factors

{- | 'oddFactors' @p@ returns all odd length factors (as lists of points) of permutation @p@. -}
oddFactors :: PP.Perm.Perm -> [[PP.Geometry.Point.Point]]
oddFactors = L.filter (odd . L.length) . factors


{- | 'maxFactors' @f@ @p@ returns the factors (as lists of points) of the permutation @p@ that
maximize the function @f@.

>>>
-}
maxFactors :: ([PP.Geometry.Point.Point] -> Bool) -> PP.Perm.Perm -> [[PP.Geometry.Point.Point]]
maxFactors f p = select $ L.dropWhile L.null [[q | q <- kFactors k p, f q] | k <- [n,n-1..1]]
  where
    n         = PP.Perm.len p
    select xs = if L.null xs then [] else L.head xs