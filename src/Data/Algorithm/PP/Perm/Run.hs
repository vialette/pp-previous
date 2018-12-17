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
  import qualified Data.Tuple    as T
  import Data.Function (on)

  import qualified Data.Algorithm.PP.Perm       as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List

  -- |'ascendingRuns' 'p'
  --
  -- >>> ascendingRuns (mkPerm [4,6,2,1,3,8,5,7])
  -- [[4,6],[2],[1,3,8],[5,7]]
  ascendingRuns :: PP.Perm.Perm -> [PP.Perm.Patt]
  ascendingRuns = L.map PP.Perm.mkPatt . PP.Utils.List.groupBy' f . PP.Perm.getPoints
    where
      f (_, y1) (_, y2) = y1 < y2

  -- |'longestAscendingRun' 'p'
  --
  -- >>> longestAscendingRun (mkPerm [4,6,2,1,3,8,5,7])
  -- [1,3,8]
  longestAscendingRun :: PP.Perm.Perm -> PP.Perm.Patt
  longestAscendingRun = F.maximumBy (compare `on` PP.Perm.len) . ascendingRuns

  -- |'descendingRuns' 'p'
  --
  -- >>> descendingRuns (mkPerm [4,6,2,1,3,8,5,7])
  -- [[4],[6,2,1],[3],[8,5],[7]]
  descendingRuns :: PP.Perm.Perm -> [PP.Perm.Patt]
  descendingRuns = L.map PP.Perm.mkPatt . PP.Utils.List.groupBy' f . PP.Perm.getPoints
    where
      f p1 p2 = T.snd p1 > T.snd p2

  -- |'longestDescendingRun' 'p'
  --
  -- >>> longestDescendingRun (mkPerm [4,6,2,1,3,8,5,7])
  -- [6,2,1]
  longestDescendingRun :: PP.Perm.Perm -> PP.Perm.Patt
  longestDescendingRun = F.maximumBy (compare `on` PP.Perm.len) . descendingRuns
