module Data.Algorithm.PP.Perm.Pattern (
    kPatterns
  , patterns
  ) where

import qualified Control.Arrow   as Arrow
import qualified Data.Foldable   as F
import Data.Function (on)
import qualified Data.List       as L
import qualified Data.Tuple      as T

import qualified Data.Algorithm.PP.Perm           as PP.Perm
import qualified Data.Algorithm.PP.Utils.Foldable as PP.Utils.Foldable
import qualified Data.Algorithm.PP.Utils.List     as PP.Utils.List

{- | 'kPatterns' @k@ @p@ function returns the list of all permutations of length
@k@ that occur in the permutation @p@.

>>> kPatterns 1 $ mkPerm [1,4,2,5,3]
[[1]]
>>> kPatterns 2 $ mkPerm [1,4,2,5,3]
[[1,2],[2,1]]
>>> kPatterns 3 $ mkPerm [1,4,2,5,3]
[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2]]
>>> kPatterns 4 $ mkPerm [1,4,2,5,3]
[[1,2,4,3],[1,3,2,4],[1,3,4,2],[1,4,2,3],[3,1,4,2]]
>>> kPatterns 5 $ mkPerm [1,4,2,5,3]
[[1,4,2,5,3]]
-}
kPatterns :: Int -> PP.Perm.Perm -> [PP.Perm.Perm]
kPatterns k = PP.Utils.List.uniq . L.map PP.Perm.mkPerm . PP.Utils.Foldable.subsets k . PP.Perm.getList

patterns :: PP.Perm.Perm -> [PP.Perm.Perm]
patterns p = [kPatterns k p | k <- [1..PP.Perm.len p]]