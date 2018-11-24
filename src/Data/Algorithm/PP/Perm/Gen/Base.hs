module Data.Algorithm.PP.Perm.Gen.Base
(
  derangements

, oscillatingIncreasing
, oscillatingIncreasings

, simpleAlternatingWedgeType1
, simpleAlternatingWedgeType1s

, simpleAlternatingWedgeType2
)
where

  import qualified Data.Foldable as F
  import qualified Data.List     as L

  import qualified Data.Algorithm.PP.Perm       as PP.Perm
  import qualified Data.Algorithm.PP.Perm.Prop  as PP.Perm.Prop
  import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List

  -- |'derangements' 'n' returns all derangements of length 'n'.
  derangements :: Int -> [PP.Perm.Perm]
  derangements = L.filter PP.Perm.Prop.derangement . PP.Perm.perms

  -- |'oscillatingIncreasing' 'n' return the oscillanting increasing permutations
  -- of length 'n'.
  --
  -- >>> oscillatingIncreasing 10
  -- [3,1,5,2,7,4,9,6,10,8]
  oscillatingIncreasing :: Int-> PP.Perm.Perm
  oscillatingIncreasing n = PP.Perm.mk . L.take n $ F.concat [[2*k+2, 2*k-1] | k <- [1..]]

  -- |'oscillatingIncreasings' return the infinite list of all oscillanting increasing
  -- permutations.
  --
  -- >>> take 8 oscillatingIncreasings
  -- [[1],[2,1],[2,1,3],[3,1,4,2],[3,1,4,2,5],[3,1,5,2,6,4],[3,1,5,2,6,4,7],[3,1,5,2,7,4,8,6]]
  oscillatingIncreasings :: [PP.Perm.Perm]
  oscillatingIncreasings = [oscillatingIncreasing n | n <- [1..]]

  f n
    | even n    = (n `div` 2, n `div` 2)
    | otherwise = (1 + (n `div` 2), n `div` 2)

  -- |'simpleAlternatingWedgeType1' 'n' return the simple alternating wedge type 1
  -- permutations of length 'n'.
  --
  -- >>> simpleAlternatingWedgeType1 12
  -- [7,5,8,4,9,3,10,2,11,1,12,6]
  simpleAlternatingWedgeType1 :: Int -> PP.Perm.Perm
  simpleAlternatingWedgeType1 n = PP.Perm.mk $ PP.Utils.List.perfectShuffle xs ys ++ [kDec+1]
    where
      (kInc, kDec) = f (n-1)
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
