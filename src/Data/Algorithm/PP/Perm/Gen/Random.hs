import Data.Algorithm.PP.Perm.Gen.Random
(
  random
)
where

  import System.Random
  import qualified Control.Arrow as A

  import qualified Data.Algorithm.PP.Perm       as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List

  random :: RandomGen g => g -> Int -> (PPP.Perm, g)
  random g n = A.first PP.Perm.mkPerm $ PP.Utils.List.randomShuffle g [1..n]
