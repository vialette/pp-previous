module Data.Algorithm.PP.Perm.Generator.Random
(
  rand
, rands

, randAvoiding_213_231
, randsAvoiding_213_231
)
where

  import System.Random
  import qualified Control.Arrow as A
  import qualified Data.List     as L

  import qualified Data.Algorithm.PP.Perm       as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List

  -- rands auxiliary function.
  randsGen :: RandomGen g => (Int -> g -> (PP.Perm.Perm, g)) -> Int -> Int -> [PP.Perm.Perm] -> g -> ([PP.Perm.Perm], g)
  randsGen f n = aux
    where
      aux k acc g
        | k <= 0    = (acc, g)
        | otherwise = aux (k-1) (p : acc) g'
          where
            (p, g') = f n g

  -- |'random' 'n' 'g' takes an integer 'n' and a random generator 'g', and it returns
  -- a random permutation of length 'n' together with a new random generator.
  --
  -- >>> rand (mkStdGen 12345) 9
  -- ([3,9,1,5,8,7,2,4,6],2014025278 1422611300)
  --
  rand :: RandomGen g => Int -> g -> (PP.Perm.Perm, g)
  rand n = A.first PP.Perm.mkPerm . PP.Utils.List.randomShuffle [1..n]

  -- |'rands' 'n' 'k' 'g' takes two integers 'n' and 'k' and a random generator 'g',
  -- and it returns 'k' random permutations of length 'n'together with a new
  -- random generator.
  --
  -- >>> rands 10 3 (mkStdGen 123)
  -- ([[2,8,3,4,9,10,6,7,5,1],[2,5,7,4,9,1,10,6,8,3],[5,6,1,7,3,9,2,10,4,8]],2109333801 1701490540)
  rands :: RandomGen g => Int -> Int -> g -> ([PP.Perm.Perm], g)
  rands n k = randsGen rand n k []

  -- |'rand213'231Avoiding' 'n' 'g' takes an integer 'n' and a random generator 'g', and it returns
  -- a random (213,231)-avoiding permutation of length 'n' together with a new random generator.
  --
  -- >>> randAvoiding_213_231 10 (mkStdGen 1234)
  -- ([10,9,8,1,7,2,6,5,4,3],522754180 1336516156)
  randAvoiding_213_231 :: RandomGen g => Int -> g -> (PP.Perm.Perm, g)
  randAvoiding_213_231 n = aux [] [1..n]
    where
      aux acc [] g = A.first (PP.Perm.mkPerm . L.reverse) (acc, g)
      aux acc xs g = case randomR (False, True) g of
                       (False, g') -> aux (L.head xs : acc) (L.tail xs) g'
                       (True,  g') -> aux (L.last xs : acc) (L.init xs) g'

  -- |'rands213'231Avoiding' 'n' 'k' 'g' takes two integers 'n' and 'k' and a random generator 'g',
  -- and it returns 'k' random (213,231)-avoiding permutations of length 'n'together with a new
  -- random generator.
  --
  -- >>> randsAvoiding_213_231 10 3 (mkStdGen 1234)
  -- ([[1,10,2,3,4,9,8,7,5,6],[1,2,10,3,4,9,8,7,5,6],[10,9,8,1,7,2,6,5,4,3]],2109333801 1701490540)
  randsAvoiding_213_231 :: RandomGen g => Int -> Int -> g -> ([PP.Perm.Perm], g)
  randsAvoiding_213_231 n k = randsGen randAvoiding_213_231 n k []

  -- randAvoiding_321 :: RandomGen g => Int -> g -> (PP.Perm.Perm, g)
  -- randAvoiding_321 n g = []
  --
  -- randsAvoiding_321 :: RandomGen g => Int -> Int -> g -> ([PP.Perm.Perm], g)
  -- randsAvoiding_321 n k = randsGen randAvoiding_321 n k []
  --
  -- randAvoiding_123 :: RandomGen g => Int -> g -> (PP.Perm.Perm, g)
  -- randAvoiding_123 n = A.first PP.Perm.rev . randAvoiding_321 n
  --
  -- randsAvoiding_123 :: RandomGen g => Int -> Int -> g -> ([PP.Perm.Perm], g)
  -- randsAvoiding_123 n k = randsGen randAvoiding_123 n k []
  --
  -- randAvoiding_132 :: RandomGen g => Int -> g -> (PP.Perm.Perm, g)
  -- randAvoiding_132 n = []
  --
  -- randsAvoiding_132 :: RandomGen g => Int -> Int -> g -> ([PP.Perm.Perm], g)
  -- randsAvoiding_132 n k = randsGen randAvoiding_123 n k []
  --
  -- randAvoiding_231 :: RandomGen g => Int -> g -> (PP.Perm.Perm, g)
  -- randAvoiding_231 n = A.first PP.Perm.rev . randAvoiding_132 n
  --
  -- randsAvoiding_231 :: RandomGen g => Int -> Int -> g -> ([PP.Perm.Perm], g)
  -- randsAvoiding_231 n k = randsGen randAvoiding_231 n k []
  --
  -- randAvoiding_312 :: RandomGen g => Int -> g -> (PP.Perm.Perm, g)
  -- randAvoiding_312 n = A.first PP.Perm.comp . randAvoiding_132 n
  --
  -- randsAvoiding_312 :: RandomGen g => Int -> Int -> g -> ([PP.Perm.Perm], g)
  -- randsAvoiding_312 n k = randsGen randAvoiding_312 n k []
  --
  -- randAvoiding_213 :: RandomGen g => Int -> g -> (PP.Perm.Perm, g)
  -- randAvoiding_213 n = A.first PP.Perm.revComp . randAvoiding_132 n
  --
  -- randsAvoiding_213 :: RandomGen g => Int -> Int -> g -> ([PP.Perm.Perm], g)
  -- randsAvoiding_213 n k = randsGen randAvoiding_213 n k []
