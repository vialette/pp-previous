{-|
Module      : Data.Algorithm.PP.Perm.Statistics
Description : Various statistics on permutations
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PP.Perm.Statistics (
    fixedPoints
  , ascents
  , descents
  , excedances
  , weakExcedances
  , peaks
  , maxima
  , valleys
  , minima
  , leftToRightMinima
  , leftToRightMaxima
  , rightToLeftMaxima
  , rightToLeftMaxima
  ) where

import qualified Data.List as L

import qualified Data.Algorithm.PP.Perm           as PP.Perm
import qualified Data.Algorithm.PP.Perm.Features  as PP.Perm.Features

{- | 'fixedPoints' @p@ returns the number of fixed points in the permutation @p@.

>>> fixedPoints $ mk [4,2,3,1,6,5,7,8]
4
>>> fixedPoints $ mk [3,1,5,6,2,4,8,7]
0
-}
fixedPoints :: PP.Perm.Perm -> Int
fixedPoints = L.length . PP.Perm.Features.fixedPoints

{- | 'ascents' @p@ returns the number of ascents in the permutation @p@.

>>> ascents $ mk [3,1,5,6,2,4]
3
-}
ascents :: PP.Perm.Perm -> Int
ascents = L.length . PP.Perm.Features.ascents

{- | 'descents' @p@ return the number of descents in the permutation @p@.

>>> descents $ mk [3,1,5,6,2,4]
2
-}
descents :: PP.Perm.Perm -> Int
descents = L.length . PP.Perm.Features.descents

{- | 'excedances' @p@ returns the number of excedences in the permutation @p@.

>>> excedances $
-}
excedances :: PP.Perm.Perm -> Int
excedances = L.length . PP.Perm.Features.excedances

{- | 'weakExcedances' @p@ returns the number of weak excedences in the permutation @p@.

>>> weakExcedances $
-}
weakExcedances :: PP.Perm.Perm -> Int
weakExcedances = L.length . PP.Perm.Features.weakExcedances

{- | 'peaks' @p@ returns the number of peaks in the permutation @p@.

>>> peaks (mk [4,6,1,3,2,5])
-- 2
-}
peaks :: PP.Perm.Perm -> Int
peaks = L.length . PP.Perm.Features.peaks

{- | Alias for 'peaks'. -}
maxima :: PP.Perm.Perm -> Int
maxima = peaks

{- | 'valleys' @p@ returns the number of valleys in the permutation @p@.

>>> valleys (mk [3,1,5,2,6,4])
2
-}
valleys :: PP.Perm.Perm -> Int
valleys = L.length . PP.Perm.Features.valleys

{- | Alias for 'valleys'. -}
minima :: PP.Perm.Perm -> Int
minima = valleys

{- | 'leftToRightMinima' @p@ returns the number of left-to-right minima in the permutation @p@.

>>> leftToRightMinima $
-}
leftToRightMinima :: PP.Perm.Perm -> Int
leftToRightMinima = L.length . PP.Perm.Features.leftToRightMinima

{- | 'leftToRightMaxima' @p@ returns the number of left-to-right maxima in the permutation @p@.

>>> leftToRightMaxima $
-}
leftToRightMaxima :: PP.Perm.Perm -> Int
leftToRightMaxima = L.length . PP.Perm.Features.leftToRightMaxima

{- | 'rightToLeftMinima' @p@ returns the number of right-to-left minima in the permutation @p@.

>>> rightToLeftMinima $
-}
rightToLeftMinima :: PP.Perm.Perm -> Int
rightToLeftMinima = L.length . PP.Perm.Features.rightToLeftMinima

{- | 'rightToLeftMaxima' @p@ returns the number of right-to-left maxima in the permutation @p@.

>>> rightToLeftMaxima $
-}
rightToLeftMaxima :: PP.Perm.Perm -> Int
rightToLeftMaxima = L.length . PP.Perm.Features.rightToLeftMaxima
