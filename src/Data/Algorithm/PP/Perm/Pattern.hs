module Data.Algorithm.PP.Perm.Pattern (
    kPatterns
  , patterns
  , evenPatterns
  , oddPatterns
  ) where

import qualified Control.Arrow   as Arrow
import qualified Data.Foldable   as F
import Data.Function (on)
import qualified Data.List       as L
import qualified Data.Tuple      as T

import qualified Data.Algorithm.PP.Perm           as PP.Perm
import qualified Data.Algorithm.PP.Utils.Foldable as PP.Utils.Foldable
import qualified Data.Algorithm.PP.Utils.List     as PP.Utils.List

{- | 'kPatterns' @k@ @p@ returns the list of all permutations of length @k@ that occur in the permutation @p@.

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
kPatterns k = PP.Utils.List.uniq . L.map PP.Perm.mk . PP.Utils.List.subsets k . PP.Perm.getList

{- | 'patterns' @p@ returns the list of all permutations that occur in the permutation @p@.


>>> patterns $ mkPerm [1,4,2,5,3]
[[1],[1,2],[2,1],[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[1,2,4,3],[1,3,2,4],[1,3,4,2],[1,4,2,3],[3,1,4,2],[1,4,2,5,3]]
-}
patterns :: PP.Perm.Perm -> [PP.Perm.Perm]
patterns p = L.concat [kPatterns k p | k <- [1..PP.Perm.len p]]

{- | 'evenPatterns' @p@ returns all even lenth patterns that ocuur in permutation @p@.
-}
evenPatterns :: PP.Perm.Perm -> [PP.Perm.Perm]
evenPatterns p = L.concat [kPatterns k p | k <- [2,4..n]]
  where
    n : PP.Perm.len p

{- | 'oddPatterns' @p@ returns all even lenth patterns that ocuur in permutation @p@.
-}
oddPatterns :: PP.Perm.Perm -> [PP.Perm.Perm]
oddPatterns p = L.concat [kPatterns k p | k <- [1,3..n]]
  where
    n : PP.Perm.len p