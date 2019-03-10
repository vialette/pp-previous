{-|
Module      : Data.Algorithm.PP.Perm.Run
Description : Runs in permutations
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Computing ascending and descending runs in permutations.
-}

module Data.Algorithm.PP.Perm.Run
  (
  -- * Ascending run
    ascendingRuns
  , longestAscendingRun
  , longestAscendingRuns

  -- * Descending run
  , descendingRuns
  , longestDescendingRun
  , longestDescendingRuns
  ) where

import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T
import Data.Function (on)

import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point
import qualified Data.Algorithm.PP.Perm           as PP.Perm
import qualified Data.Algorithm.PP.Utils.Foldable as PP.Utils.Foldable
import qualified Data.Algorithm.PP.Utils.List     as PP.Utils.List

{- |'ascendingRuns' @p@ returns all ascending runs (as lists of points) in permutation @p@.

A sorted permutation (/i.e./ [1,2,...,n]) consists of a single ascending run,
whereas a reverse sorted permutation (/i.e./ [n,n-1,...1]) consists of n ascending
runs, each of length 1.

>>> ascendingRuns $ mkPerm [1..9]
[[1,2,3,4,5,6,7,8,9]]
>>> ascendingRuns $ mkPerm [9,8..1]
[[9],[8],[7],[6],[5],[4],[3],[2],[1]]
>>> ascendingRuns $ mkPerm [2,4,6,1,3,9,8,5,7]
[[2,4,6],[1,3,9],[8],[5,7]]
-}
ascendingRuns :: PP.Perm.Perm -> [[PP.Geometry.Point.Point]]
ascendingRuns = PP.Utils.List.groupBy' PP.Geometry.Point.isStrictlyBelowOf . PP.Perm.getPoints

{- |'longestAscendingRun' @p@ returns a longest ascending run (as a list of points) in permutation @p@.

>>> longestAscendingRun $ mkPerm [1..9]
[1,2,3,4,5,6,7,8,9]
>>> longestAscendingRun $ mkPerm [9,8..1]
[1]
>>> longestAscendingRun $ mkPerm [2,4,6,1,3,9,8,5,7]
[1,3,9]
-}
longestAscendingRun :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
longestAscendingRun = F.maximumBy (compare `on` L.length) . ascendingRuns

{- |'longestAscendingRuns' @p@ returns all longest ascending runs (as lists of points) in permutation @p@.

>>> longestAscendingRuns $ mkPerm [1..9]
[[1,2,3,4,5,6,7,8,9]]
>>> longestAscendingRuns $ mkPerm [9,8..1]
[[9],[8],[7],[6],[5],[4],[3],[2],[1]]
>>> longestAscendingRuns $ mkPerm [2,4,6,1,3,9,8,5,7]
[[2,4,6],[1,3,9]]
-}
longestAscendingRuns :: PP.Perm.Perm -> [[PP.Geometry.Point.Point]]
longestAscendingRuns = PP.Utils.Foldable.maximumsBy L.length . ascendingRuns

{- |'descendingRuns' @p@ returns all descending runs (as lists of points) in permutation @p@.

A reverse sorted permutation (/i.e./ [n,n-1,...1]) consists of a single descending run,
whereas a sorted permutation (/i.e./ [1,2,...,n]) consists of n descending
runs, each of length 1.

>>> descendingRuns $ mkPerm [1..9]
[[1],[2],[3],[4],[5],[6],[7],[8],[9]]
>>> descendingRuns $ mkPerm [9,8..1]
[[9,8,7,6,5,4,3,2,1]]
>>> descendingRuns $ mkPerm [7,3,1,6,5,2,4,9,8]
[[7,3,1],[6,5,2],[4],[9,8]]
-}
descendingRuns :: PP.Perm.Perm -> [[PP.Geometry.Point.Point]]
descendingRuns = PP.Utils.List.groupBy' f . PP.Perm.getPoints
  where
    f p1 p2 = PP.Geometry.Point.getY p1 > PP.Geometry.Point.getY p2

{- |'longestDescendingRun' @p@ returns a longest descending run (as a list of points) in permutation
@p@.

>>> longestDescendingRun $ mkPerm [1..9]
[9]
>>> longestDescendingRun $ mkPerm [9,8..1]
[9,8,7,6,5,4,3,2,1]
>>> longestDescendingRun $ mkPerm [7,3,1,6,5,2,4,9,8]
[6,5,2]
-}
longestDescendingRun :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
longestDescendingRun = F.maximumBy (compare `on` L.length) . descendingRuns

{- |'longestAscendingRuns' @p@ returns all longest ascending runs (as lists of points) in permutation @p@.

>>> longestDescendingRuns $ mkPerm [1..9]
[[1],[2],[3],[4],[5],[6],[7],[8],[9]]
>>> longestDescendingRuns $ mkPerm [9,8..1]
[[9,8,7,6,5,4,3,2,1]]
>>> longestDescendingRuns $ mkPerm [7,3,1,6,5,2,4,9,8]
[[7,3,1],[6,5,2]]
-}
longestDescendingRuns :: PP.Perm.Perm -> [[PP.Geometry.Point.Point]]
longestDescendingRuns = PP.Utils.Foldable.maximumsBy L.length . descendingRuns
