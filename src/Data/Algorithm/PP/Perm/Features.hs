{-|
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
, rightmost
, fixedPoints
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


-- |'leftmost' 'p' returns the first (i.e. leftmost) element of the
-- permutation 'p'.
--
-- >>> leftmost (mkPerm [1..4])
-- Just 1
-- >>> lelftmost empty
-- Nothing
leftmost :: PP.Perm.Perm -> Maybe Int
leftmost = PP.Utils.List.safeHead . PP.Perm.getList

-- |'rightmost' 'p' returns the last (i.e. rightmost) element of the
-- permutation 'p'.
--
-- >>> rightmost (mkPerm [1..4])
-- Just 4
-- >>> rightmost empty
-- Nothing
rightmost :: PP.Perm.Perm -> Maybe Int
rightmost = PP.Utils.List.safeLast . PP.Perm.getList

-- |'fixedPoints' 'p' returns the fixed points in the permutation 'p'.
--
-- >>> fixedPoints (mkPerm [4,2,3,1,6,5,7,8])
-- [(2,2),(3,3),(7,7),(8,8)]
fixedPoints :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
fixedPoints = L.filter PP.Geometry.Point.isOnDiagonal . PP.Perm.getPoints

-- |'ascents' 'p' returns the ascents of the permutation 'p'
-- (i.e. the positions of 'p' where the following value is bigger than the current
-- one).
--
-- >>> ascents (mkPerm [3,1,5,6,2,4])
-- [(2,1),(3,5),(5,2)]
ascents :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
ascents = L.map T.fst . L.filter (uncurry PP.Geometry.Point.isStrictlyBelowOf) . PP.Utils.List.chunk2 . PP.Perm.getPoints

-- |'descents' 'p' returns the ascents of the permutation 'p'
-- (i.e. the positions of 'p' where the following value is smaller than the current
-- one).
--
-- >>> descents (mkPerm [3,1,5,6,2,4])
-- [(1,3),(4,6)]
descents :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
descents =  L.map T.fst . L.filter (uncurry PP.Geometry.Point.isStrictlyAboveOf) . PP.Utils.List.chunk2 . PP.Perm.getPoints


-- |'excedances' 'p' returns the excedancesof the permutation 'p'.
-- (i.e. the excedance set is the set of indices i for which @p `at` i > i.
--
-- >>>
excedances :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
excedances = L.filter PP.Geometry.Point.isStrictlyAboveDiagonal . PP.Perm.getPoints

-- |'weakExcedances' 'p'
--
-- >>>
weakExcedances :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
weakExcedances =  L.filter PP.Geometry.Point.isAboveDiagonal . PP.Perm.getPoints

-- |'peaks' 'p'
--
-- >>> peaks (mk [4,6,1,3,2,5])
-- [(2,6),(4,3)]
peaks :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
peaks = L.map proj2 . L.filter f . PP.Utils.List.chunk3 . PP.Perm.getPoints
  where
    f (p1, p2, p3)   = p1 `PP.Geometry.Point.isStrictlyBelowOf` p2 && p2 `PP.Geometry.Point.isStrictlyAboveOf` p3
    proj2 (_, p2, _) = p2

-- |Alias for 'peaks'.
maxima :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
maxima = peaks

-- |'valleys' 'p'
--
-- >>> valleys (mk [3,1,5,2,6,4])
-- [(2,1),(4,2)]
valleys :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
valleys = L.map proj2 . L.filter f . PP.Utils.List.chunk3 . PP.Perm.getPoints
  where
    f (p1, p2, p3)   = p1 `PP.Geometry.Point.isStrictlyAboveOf` p2 && p2 `PP.Geometry.Point.isStrictlyBelowOf` p3
    proj2 (_, p2, _) = p2

-- | Alias for 'valleys'.
minima :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
minima = valleys

-- |'leftToRightMinima' 'p'
leftToRightMinima :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
leftToRightMinima = L.reverse . F.foldr f [] . L.reverse . PP.Perm.getPoints
  where
    f p [] = [p]
    f p acc@(p' : _)
      | p `PP.Geometry.Point.isStrictlyBelowOf` p' = p : acc
      | otherwise                                  = acc


-- |'leftToRightMaxima' 'p'
--
-- >>> leftToRightMaxima (mk [4,2,3,1,6,5,7,8])
-- [(1,4),(5,6),(7,7),(8,8)]
leftToRightMaxima :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
leftToRightMaxima = L.reverse . F.foldr f [] . L.reverse . PP.Perm.getPoints
  where
    f p [] = [p]
    f p acc@(p' : _)
      | p `PP.Geometry.Point.isStrictlyAboveOf`p' = p : acc
      | otherwise                                 = acc

-- |'rightToLeftMaxima' 'p'
--
-- >>> rightToLeftMinima (mk [3,1,5,2,6,4])
-- [(2,6),(1,4)]
rightToLeftMinima :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
rightToLeftMinima = L.reverse . leftToRightMaxima . PP.Perm.Bijection.Trivial.rev

-- |'rightToLeftMaxima' 'p'
--
-- >>> rightToLeftMaxima (mk [3,1,5,2,6,4])
-- [(2,6),(1,4)]
rightToLeftMaxima :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
rightToLeftMaxima = L.reverse . leftToRightMaxima . PP.Perm.Bijection.Trivial.rev
