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
  -- * Ascending runs
    ascendingRuns
  , longestAscendingRuns

  -- * Descending runs
  , descendingRuns
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

{- | 'ascendingRuns' @p@ returns all ascending runs (as lists of points) in permutation @p@.

A sorted permutation (/i.e./ [1,2,...,n]) consists of a single ascending run,
whereas a reverse sorted permutation (/i.e./ [n,n-1,...1]) consists of n ascending
runs, each of length 1.

>>> let p = mk [1..8] in ascendingRuns p
[[(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7),(8,8)]]
>>> let p = mk [8,7..1] in ascendingRuns p
[[(1,8)],[(2,7)],[(3,6)],[(4,5)],[(5,4)],[(6,3)],[(7,2)],[(8,1)]]
>>> let p = mk [2,4,6,1,3,8,5,7] in ascendingRuns p
[[(1,2),(2,4),(3,6)],[(4,1),(5,3),(6,8)],[(7,5),(8,7)]]
-}
ascendingRuns :: PP.Perm.Perm -> [[PP.Geometry.Point.Point]]
ascendingRuns = PP.Utils.List.groupBy' PP.Geometry.Point.isStrictlyBelowOf . PP.Perm.getPoints

{- | 'longestAscendingRuns' @p@ returns all longest ascending runs (as lists of points) in
permutation @p@.

>>> let p = mk [1..8] in longestAscendingRuns p
[[(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7),(8,8)]]
>>> let p = mk [8,7..1] in longestAscendingRuns p
[[(1,8)],[(2,7)],[(3,6)],[(4,5)],[(5,4)],[(6,3)],[(7,2)],[(8,1)]]
>>> let p = mk [2,4,7,6,1,3,8,5] in longestAscendingRuns p
[[(1,2),(2,4),(3,7)],[(5,1),(6,3),(7,8)]]
-}
longestAscendingRuns :: PP.Perm.Perm -> [[PP.Geometry.Point.Point]]
longestAscendingRuns = T.snd . PP.Utils.Foldable.maximumsBy L.length . ascendingRuns

{- | 'descendingRuns' @p@ returns all descending runs (as lists of points) in permutation @p@.

A reverse sorted permutation (/i.e./ [n,n-1,...1]) consists of a single descending run,
whereas a sorted permutation (/i.e./ [1,2,...,n]) consists of n descending
runs, each of length 1.

>>> let p = mk [1..8] in descendingRuns p
[[(1,1)],[(2,2)],[(3,3)],[(4,4)],[(5,5)],[(6,6)],[(7,7)],[(8,8)]]
>>> let p = mk [8,7..1] in descendingRuns p
[[(1,8),(2,7),(3,6),(4,5),(5,4),(6,3),(7,2),(8,1)]]
>>> let p = mk [2,4,6,1,3,8,5,7] in descendingRuns p
[[(1,2)],[(2,4)],[(3,6),(4,1)],[(5,3)],[(6,8),(7,5)],[(8,7)]]
-}
descendingRuns :: PP.Perm.Perm -> [[PP.Geometry.Point.Point]]
descendingRuns = PP.Utils.List.groupBy' f . PP.Perm.getPoints
  where
    f p1 p2 = PP.Geometry.Point.getY p1 > PP.Geometry.Point.getY p2

{- | 'longestAscendingRuns' @p@ returns all longest ascending runs (as lists of points) in
permutation @p@.

>>> let p = mk [1..8] in longestDescendingRuns p
[[(1,1)],[(2,2)],[(3,3)],[(4,4)],[(5,5)],[(6,6)],[(7,7)],[(8,8)]]
>>> let p = mk [8,7..1] in longestDescendingRuns p
[[(1,8),(2,7),(3,6),(4,5),(5,4),(6,3),(7,2),(8,1)]]
>>> let p = mk [2,4,7,6,1,3,8,5] in longestDescendingRuns p
[[(3,7),(4,6),(5,1)]]
-}
longestDescendingRuns :: PP.Perm.Perm -> [[PP.Geometry.Point.Point]]
longestDescendingRuns = T.snd . PP.Utils.Foldable.maximumsBy L.length . descendingRuns
