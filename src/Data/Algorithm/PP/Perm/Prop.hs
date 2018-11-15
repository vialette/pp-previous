module Data.Algorithm.PP.Perm.Prop
(
  isIncreasing
, isDecreasing
, isMonotone

, isUpDownAlternating
, isDownUpAlternating
, isAlternating

, isUpDown
, isDownUp
)
where

  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Tuple    as T

  import qualified Data.Algorithm.PP.Perm as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List

  isIncreasing :: PP.Perm.Perm -> Bool
  isIncreasing = F.any f . PP.Utils.List.chunk2 . PP.Perm.toList
    where
      f [i, j] = i < j

  isDecreasing :: PP.Perm.Perm -> Bool
  isDecreasing = F.any f . PP.Utils.List.chunk2 . PP.Perm.toList
    where
      f [i, j] = i > j

  isMonotone :: PP.Perm.Perm -> Bool
  isMonotone p = isIncreasing p || isDecreasing p

  isUpDownAlternating :: PP.Perm.Perm -> Bool
  isUpDownAlternating = isUpDownAlternatingT . PP.Utils.List.chunk3 . PP.Perm.toList

  isUpDownAlternatingT :: [[PP.Perm.T]] -> Bool
  isUpDownAlternatingT []                 = True
  isUpDownAlternatingT ([i, j, k] : ijks) = i < j && j > k && isDownUpAlternatingT ijks

  isDownUpAlternating :: PP.Perm.Perm -> Bool
  isDownUpAlternating = isDownUpAlternatingT . PP.Utils.List.chunk3 . PP.Perm.toList

  isDownUpAlternatingT :: [[PP.Perm.T]] -> Bool
  isDownUpAlternatingT []                 = True
  isDownUpAlternatingT ([i, j, k] : ijks) = i > j && j < k && isUpDownAlternatingT ijks

  isAlternating :: PP.Perm.Perm -> Bool
  isAlternating p = isUpDownAlternating || isDownUpAlternating p
