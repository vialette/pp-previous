{-|
Module      : Data.Algorithm.PP.Perm.Run
Description : Runs in permutations
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module Data.Algorithm.PP.Perm.Run
(
  ascendingRuns
, longestAscendingRun

, descendingRuns
, longestDescendingRun
)
where

  import qualified Data.Foldable as F
  import qualified Data.List     as L

  import Data.Algorithm.PP.Geometry.Point ((@<|), (@>|))
  import Data.Function (on)
  import qualified Data.Algorithm.PP.Perm       as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List

  -- |'ascendingRuns' 'p'
  --
  -- >>> ascendingRuns (mk [4,6,2,1,3,8,5,7])
  -- [[4,6],[2],[1,3,8],[5,7]]
  ascendingRuns :: PP.Perm.Perm -> [PP.Perm.Pattern]
  ascendingRuns = L.map PP.Perm.fromPointsUnsafe . PP.Utils.List.groupBy' (@<|) . PP.Perm.getPoints

  -- |'longestAscendingRun' 'p'
  --
  -- >>> longestAscendingRun (mk [4,6,2,1,3,8,5,7])
  -- [1,3,8]
  longestAscendingRun :: PP.Perm.Perm -> PP.Perm.Pattern
  longestAscendingRun = F.maximumBy (compare `on` PP.Perm.len) . ascendingRuns

  -- |'descendingRuns' 'p'
  --
  -- >>> descendingRuns (mk [4,6,2,1,3,8,5,7])
  -- [[4],[6,2,1],[3],[8,5],[7]]
  descendingRuns :: PP.Perm.Perm -> [PP.Perm.Pattern]
  descendingRuns = L.map PP.Perm.fromPointsUnsafe . PP.Utils.List.groupBy' (@>|) . PP.Perm.getPoints

  -- |'longestDescendingRun' 'p'
  --
  -- >>> longestDescendingRun (mk [4,6,2,1,3,8,5,7])
  -- [6,2,1]
  longestDescendingRun :: PP.Perm.Perm -> PP.Perm.Pattern
  longestDescendingRun = F.maximumBy (compare `on` PP.Perm.len) . descendingRuns
