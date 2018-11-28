{-|
Module      : Data.Algorithm.PP.Perm.Stat
Description : Various statistics on permutations
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module Data.Algorithm.PP.Perm.Stat
(
  fixedPoints
, fixedPointsStat

, ascents
, ascentsStat

, descents
, descentsStat

, excedances
, excedancesStat
, weakExcedances
, weakExcedancesStat

, peaks
, peaksStat
, maxima
, maximaStat

, valleys
, valleysStat
, minima
, minimaStat

, leftToRightMaxima
, leftToRightMaximaStat

, rightToLeftMaxima
, rightToLeftMaximaStat
)
where

  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Tuple    as T

  import Data.Algorithm.PP.Geometry.Point ((@<-), (@>-), (@<|), (@>|))
  import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point
  import qualified Data.Algorithm.PP.Perm           as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List     as PP.Utils.List

  -- |'fixedPoints' 'p' returns the fixed points in the permutation 'p'.
  --
  -- >>> fixedPoints (mk [4,2,3,1,6,5,7,8])
  -- [(2,2),(3,3),(7,7),(8,8)]
  fixedPoints :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
  fixedPoints = L.filter PP.Geometry.Point.diag. PP.Perm.getPoints

  -- |'fixedPointsStat' 'p' returns the number of fixed points in the permutation
  -- 'p'.
  --
  -- >>> fixedPointsStat (mk [4,2,3,1,6,5,7,8])
  -- [(2,2),(3,3),(7,7),(8,8)]
  -- >>> fixedPointsStat (mk [3,1,5,6,2,4,8,7])
  -- 0
  fixedPointsStat :: PP.Perm.Perm -> Int
  fixedPointsStat = L.length . fixedPoints

  -- |'ascents' 'p' returns the ascents of the permutation 'p'
  -- (i.e. the positions of 'p' where the following value is bigger than the current
  -- one).
  --
  -- >>> ascents (mk [3,1,5,6,2,4])
  -- [(2,1),(3,5),(5,2)]
  ascents :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
  ascents = L.map T.fst . L.filter (uncurry (@<-)) . PP.Utils.List.chunk2 . PP.Perm.getPoints

  -- |'ascentsStat' 'p'.
  --
  -- >>> ascentsStat (mk [3,1,5,6,2,4])
  -- 3
  ascentsStat :: PP.Perm.Perm -> Int
  ascentsStat = L.length . ascents

  -- |'descents' 'p' returns the ascents of the permutation 'p'
  -- (i.e. the positions of 'p' where the following value is smaller than the current
  -- one).
  --
  -- >>> descents (mk [3,1,5,6,2,4])
  -- [(1,3),(4,6)]
  descents :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
  descents =  L.map T.fst . L.filter (uncurry (@>-)) . PP.Utils.List.chunk2 . PP.Perm.getPoints

  -- |'descentsStat' 'p'.
  --
  -- >>> descentsStat (mk [3,1,5,6,2,4])
  -- 2
  descentsStat :: PP.Perm.Perm -> Int
  descentsStat = L.length . descents

  -- |'excedances' 'p'
  --
  -- >>>
  excedances :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
  excedances = L.filter PP.Geometry.Point.aboveDiag . PP.Perm.getPoints

  -- |'excedancesStat' 'p'
  excedancesStat :: PP.Perm.Perm -> Int
  excedancesStat = L.length . excedances

  -- |'weakExcedances' 'p'
  weakExcedances :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
  weakExcedances =  L.filter (\p -> PP.Geometry.Point.aboveDiag p || PP.Geometry.Point.diag p) . PP.Perm.getPoints

  -- |'excedancesStat' 'p'
  weakExcedancesStat :: PP.Perm.Perm -> Int
  weakExcedancesStat = L.length . weakExcedances

  -- |'peaks' 'p'
  --
  -- >>> peaks (mk [4,6,1,3,2,5])
  -- [(2,6),(4,3)]
  peaks :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
  peaks = L.map proj2 . L.filter f . PP.Utils.List.chunk3 . PP.Perm.getPoints
    where
      f (p1, p2, p3)  = p1 @<| p2 && p2 @>| p3
      proj2 (_, j, _) = j

  -- |'peaksStat' 'p'
  --
  -- >>> peaksStat (mk [4,6,1,3,2,5])
  -- 2
  peaksStat :: PP.Perm.Perm -> Int
  peaksStat = L.length . peaks

  -- |Alias for 'peaks'.
  maxima :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
  maxima = peaks

  -- | Alias for 'peaksStat'.
  maximaStat :: PP.Perm.Perm -> Int
  maximaStat = peaksStat

  -- |'valleys' 'p'
  --
  -- >>> valleys (mk [3,1,5,2,6,4])
  -- [(2,1),(4,2)]
  valleys :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
  valleys = L.map proj2 . L.filter f . PP.Utils.List.chunk3 . PP.Perm.getPoints
    where
      f (p1, p2, p3)  = p1 @>| p2 && p2 @<| p3
      proj2 (_, j, _) = j

  -- |'valleysStat' 'p'
  --
  -- >>> valleysStat(mk [3,1,5,2,6,4])
  -- 2
  valleysStat :: PP.Perm.Perm -> Int
  valleysStat = L.length . valleys

  -- | Alias for 'valleys'.
  minima :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
  minima = valleys

  -- | Alias for 'valleysStat'.
  minimaStat :: PP.Perm.Perm -> Int
  minimaStat = valleysStat

  -- |'leftToRightMinima' 'p'
  --
  -- >>> leftToRightMaxima (mk [4,2,3,1,6,5,7,8])
  -- [(1,4),(5,6),(7,7),(8,8)]
  leftToRightMaxima :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
  leftToRightMaxima = L.reverse . F.foldr f [] . L.reverse . PP.Perm.getPoints
    where
      f p [] = [p]
      f p acc@(p' : _)
        | p @>| p'  = p : acc
        | otherwise = acc

  -- |'leftToRightMaximaStat' 'p'
  --
  -- >>> leftToRightMaximaStat (mk [4,2,3,1,6,5,7,8])
  -- 3
  leftToRightMaximaStat :: PP.Perm.Perm -> Int
  leftToRightMaximaStat = L.length . leftToRightMaxima

  -- |'rightToLeftMaxima' 'p'
  --
  -- >>> rightToLeftMaxima (mk [3,1,5,2,6,4])
  -- [(2,6),(1,4)]
  rightToLeftMaxima :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
  rightToLeftMaxima = L.reverse . leftToRightMaxima . PP.Perm.rev

  -- |'rightToLeftMaximaStat' 'p'
  --
  -- >>> rightToLeftMaximaStat (mk [3,1,5,2,6,4])
  -- 2
  rightToLeftMaximaStat :: PP.Perm.Perm -> Int
  rightToLeftMaximaStat = L.length . rightToLeftMaxima
