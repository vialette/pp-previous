{- |
Module      : Data.Algorithm.PP.Perm.Features
Description : Various features of permutations
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PP.Perm.Features (
    leftmost
  , safeLeftmost
  , rightmost
  , safeRightmost
  , fixedPoints
  , ascents
  , descents
  , doubleRises
  , doubleFalls
  , excedances
  , weakExcedances
  , peaks
  , maxima

  , valleys
  , minima

  , leftToRightMinima
  , leftToRightMaxima
  , rightToLeftMinima
  , rightToLeftMaxima
  ) where

import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T

import qualified Data.Algorithm.PP.Geometry.Point         as PP.Geometry.Point
import qualified Data.Algorithm.PP.Perm                   as PP.Perm
import qualified Data.Algorithm.PP.Perm.Bijection.Trivial as PP.Perm.Bijection.Trivial
import qualified Data.Algorithm.PP.Utils.List             as PP.Utils.List


{- | 'leftmost' @p@ returns the first (/i.e./ leftmost) element of the permutation @p@.
The function raises an exception if @p@ is the empty permutation.

>>> let p = mk [3,1,4,2] in leftmost p
Just 3
>>> leftmost empty
*** Exception: Prelude.head: empty list
-}
leftmost :: PP.Perm.Perm -> Int
leftmost = L.head . PP.Perm.getList

{- | 'safeLeftmost' @p@ returns the first (/i.e./ leftmost) element of the permutation @p@.
The function returns @Nothing@ if @p@ is the empty permutation.

>>> let p = mk [3,1,4,2] in safeLeftmost p
Just 3
>>> safeLeftmost empty
Nothing
-}
safeLeftmost :: PP.Perm.Perm -> Maybe Int
safeLeftmost = PP.Utils.List.safeHead . PP.Perm.getList

{- | 'rightmost' @p@ returns the first (/i.e./ leftmost) element of the permutation @p@.
The function raises an exception if @p@ is the empty permutation.

>>> let p = mk [3,1,4,2] in rightmost p
Just 3
>>> rightmost empty
*** Exception: Prelude.head: empty list
-}
rightmost :: PP.Perm.Perm -> Int
rightmost = L.last . PP.Perm.getList

{- | 'safeRightmost' @p@ returns the last (i.e. rightmost) element of the permutation @p@.
The function returns @Nothing@ if @p@ is the empty permutation.

>>> let p = [3,1,4,2] in safeRightmost p
Just 2
>>> safeRightmost empty
Nothing
-}
safeRightmost :: PP.Perm.Perm -> Maybe Int
safeRightmost = PP.Utils.List.safeLast . PP.Perm.getList

{- | 'fixedPoints' @p@ returns the fixed points (as a list of points) in the permutation @p@.

>>> let p = [4,2,3,1,6,5,7,8] in fixedPoints p
[(2,2),(3,3),(7,7),(8,8)]
-}
fixedPoints :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
fixedPoints = L.filter PP.Geometry.Point.isOnDiagonal . PP.Perm.getPoints

{- | 'ascents' @p@ returns the ascents (as a list of points) of permutation @p@
(/i.e./ the positions of @p@ where the following value is bigger than the current one).

>>> let p = mk [3,1,5,6,2,4] in ascents p
[(2,1),(3,5),(5,2)]
-}
ascents :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
ascents = L.map T.fst . L.filter (uncurry PP.Geometry.Point.isStrictlyBelowOf) . PP.Utils.List.chunk2 . PP.Perm.getPoints

{- | 'descents' @p@ returns the ascents (as a list of points) of permutation @p@
(/i.e./ the positions of @p@ where the following value is smaller than the current one).

>>> let p = mk [3,1,5,6,2,4] in descents p
[(1,3),(4,6)]
-}
descents :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
descents =  L.map T.fst . L.filter (uncurry PP.Geometry.Point.isStrictlyAboveOf) . PP.Utils.List.chunk2 . PP.Perm.getPoints

{- | 'doubleRises' @p@ returns the double rises (as a list of points) of permutation @p@
(/i.e./ the positions of @p@ where the preceding value is smaller and the following value is
bigger than the current one).

>>> let p = mk [3,5,7,8,1,2,4,6] in doubleRises p
[(2,5),(3,7),(6,2),(7,4)]
-}
doubleRises :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
doubleRises =  L.map (\ (_, x, _) -> x) . L.filter f . PP.Utils.List.chunk3 . PP.Perm.getPoints
  where
    f (x, y, z) = x `PP.Geometry.Point.isStrictlyBelowOf` y && y `PP.Geometry.Point.isStrictlyBelowOf` z

{- | 'doubleFalls' @p@ returns the double falls (as a list of points) of permutation @p@
(/i.e./ the positions of @p@ where the preceding value is bigger and the following value is
smaller than the current one).

>>> let p = mk [4,3,2,1,8,7,6,5] in doubleFalls p
[(2,3),(3,2),(6,7),(7,6)]
-}
doubleFalls :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
doubleFalls =  L.map (\ (_, x, _) -> x) . L.filter f . PP.Utils.List.chunk3 . PP.Perm.getPoints
  where
    f (x, y, z) = x `PP.Geometry.Point.isStrictlyAboveOf` y && y `PP.Geometry.Point.isStrictlyAboveOf` z

{- | 'excedances' @p@ returns the excedances (as a list of points) of the permutation @p@.
(/i.e./ the excedance set is the set of indices @i@ for which @p `at` i > i@.

>>> let p = mk [3,2,1,5,6,4] in excedances p
[(1,3),(4,5),(5,6)]
-}
excedances :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
excedances = L.filter PP.Geometry.Point.isStrictlyAboveDiagonal . PP.Perm.getPoints

{- | 'weakExcedances' @p@ returns the weak excedances (as a list of points) of the permutation @p@.
(/i.e./ the excedance set is the set of indices @i@ for which @p `at` i >= i@.

>>> let p = mk [3,2,1,5,6,4] in weakExcedances p
[(1,3),(2,2),(4,5),(5,6)]
-}
weakExcedances :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
weakExcedances =  L.filter PP.Geometry.Point.isAboveDiagonal . PP.Perm.getPoints

{- | 'peaks' @p@ returns (as a list of points) the peaks of permutation @p@.
Position @i@ is a peak if the elements at position @i-1@ and @i+1@ are smaller.

>>> let p = mk [4,6,1,3,2,5] in peaks p
[(2,6),(4,3)]
-}
peaks :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
peaks = L.map proj2 . L.filter f . PP.Utils.List.chunk3 . PP.Perm.getPoints
  where
    f (p1, p2, p3)   = p1 `PP.Geometry.Point.isStrictlyBelowOf` p2 && p2 `PP.Geometry.Point.isStrictlyAboveOf` p3
    proj2 (_, p2, _) = p2

-- |Alias for 'peaks'.
maxima :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
maxima = peaks

{- | 'valleys' @p@ returns (as a list of points) the valleys of permutation @p@.
Position @i@ is a valley if the elements at position @i-1@ and @i+1@ are bigger.


>>> let p = mk [4,6,1,3,2,5] in valleys p
[(3,1),(5,2)]
-}
valleys :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
valleys = L.map proj2 . L.filter f . PP.Utils.List.chunk3 . PP.Perm.getPoints
  where
    f (p1, p2, p3)   = p1 `PP.Geometry.Point.isStrictlyAboveOf` p2 && p2 `PP.Geometry.Point.isStrictlyBelowOf` p3
    proj2 (_, p2, _) = p2

-- | Alias for 'valleys'.
minima :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
minima = valleys

{- | 'leftToRightMinima' @p@

>>> let p = mk [4,6,1,3,2,5] in leftToRightMinima p
[(1,4),(3,1)]
-}
leftToRightMinima :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
leftToRightMinima = L.reverse . F.foldr f [] . L.reverse . PP.Perm.getPoints
  where
    f p [] = [p]
    f p acc@(p' : _)
      | p `PP.Geometry.Point.isStrictlyBelowOf` p' = p : acc
      | otherwise                                  = acc

{- | 'leftToRightMaxima' @p@

>>> let p = mk [4,6,1,3,2,5] in leftToRightMaxima p
[(1,4),(2,6)]
-}
leftToRightMaxima :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
leftToRightMaxima = L.reverse . F.foldr f [] . L.reverse . PP.Perm.getPoints
  where
    f p [] = [p]
    f p acc@(p' : _)
      | p `PP.Geometry.Point.isStrictlyAboveOf`p' = p : acc
      | otherwise                                 = acc

{- | 'rightToLeftMinima' @p@

>>> let p = mk [4,6,1,3,2,5] in rightToLeftMinima p
[(5,6),(1,5)]
-}
rightToLeftMinima :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
rightToLeftMinima = L.reverse . leftToRightMaxima . PP.Perm.Bijection.Trivial.rev

{- | 'rightToLeftMaxima' @p@

>>> let p = mk [4,6,1,3,2,5] in rightToLeftMaxima p
[(5,6),(1,5)]
-}
rightToLeftMaxima :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
rightToLeftMaxima = L.reverse . leftToRightMaxima . PP.Perm.Bijection.Trivial.rev
