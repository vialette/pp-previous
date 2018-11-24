module Data.Algorithm.PP.Perm.Prop
(
  derangement

, increasing
, decreasing
, monotone

, upDownAlternating
, downUpAlternating
, alternating
)
where

  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Tuple    as T

  import qualified Data.Algorithm.PP.Perm as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List


  derangement :: PP.Perm.Perm -> Bool
  derangement = F.all (T.uncurry (/=)) . L.zip [1..] . PP.Perm.toList

  -- |
  increasing :: PP.Perm.Perm -> Bool
  increasing = F.any f . PP.Utils.List.chunk2 . PP.Perm.toList
    where
      f [i, j] = i < j

  -- |
  decreasing :: PP.Perm.Perm -> Bool
  decreasing = F.any f . PP.Utils.List.chunk2 . PP.Perm.toList
    where
      f [i, j] = i > j

  -- |
  monotone :: PP.Perm.Perm -> Bool
  monotone p = increasing p || decreasing p

  -- |
  upDownAlternating' :: [[PP.Perm.T]] -> Bool
  upDownAlternating' []                 = True
  upDownAlternating' ([i, j, k] : ijks) = i < j && j > k && downUpAlternating' ijks

  -- |
  downUpAlternating' :: [[PP.Perm.T]] -> Bool
  downUpAlternating' []                 = True
  downUpAlternating' ([i, j, k] : ijks) = i > j && j < k && upDownAlternating' ijks

  -- |
  upDownAlternating :: PP.Perm.Perm -> Bool
  upDownAlternating = upDownAlternating' . PP.Utils.List.chunk3 . PP.Perm.toList

  -- |
  downUpAlternating :: PP.Perm.Perm -> Bool
  downUpAlternating = downUpAlternating' . PP.Utils.List.chunk3 . PP.Perm.toList  --

  -- |
  alternating :: PP.Perm.Perm -> Bool
  alternating p = upDownAlternating p || downUpAlternating p
