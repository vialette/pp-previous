module Data.Algorithm.PP.Perm.Generator.Alternating
(
  simpleAlternatingWedgeType1
, simpleAlternatingWedgeType1s

, simpleAlternatingWedgeType2
, simpleAlternatingWedgeType2s
)
where

  import qualified Data.Algorithm.PP.Perm       as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List

  -- |'simpleAlternatingWedgeType1' 'n' return the simple alternating wedge type 1
  -- permutations of length 'n'.
  --
  -- >>> simpleAlternatingWedgeType1 12
  -- [7,5,8,4,9,3,10,2,11,1,12,6]
  simpleAlternatingWedgeType1 :: Int -> PP.Perm.Perm
  simpleAlternatingWedgeType1 n = PP.Perm.mk $ PP.Utils.List.perfectShuffle xs ys ++ [kDec+1]
    where
      (kInc, kDec) = if even (n-1) then (n `div` 2, n `div` 2) else (1 + (n `div` 2), n `div` 2)
      xs           = [kDec+2..n]
      ys           = [kDec,kDec-1..1]

  -- |'simpleAlternatingWedgeType1s' return the infinite list of all simple
  -- alternating wedge type 1 permutations.
  --
  -- >>> take 8 simpleAlternatingWedgeType1s
  -- [[1],[2,1],[3,1,2],[3,1,4,2],[4,2,5,1,3],[4,2,5,1,6,3],[5,3,6,2,7,1,4],[5,3,6,2,7,1,8,4]]
  simpleAlternatingWedgeType1s :: [PP.Perm.Perm]
  simpleAlternatingWedgeType1s = [simpleAlternatingWedgeType1 n | n <- [1..]]

  simpleAlternatingWedgeType2 :: Int -> PP.Perm.Perm
  simpleAlternatingWedgeType2 = PP.Perm.identity

  simpleAlternatingWedgeType2s :: [PP.Perm.Perm]
  simpleAlternatingWedgeType2s = [simpleAlternatingWedgeType2 n | n <- [1..]]
