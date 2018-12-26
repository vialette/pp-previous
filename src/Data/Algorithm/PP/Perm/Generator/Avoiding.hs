module Data.Algorithm.PP.Perm.Generator.Avoiding
(
  -- * Avoiding a pattern of length 3
  permsAvoiding_123
, permsAvoiding_132
, permsAvoiding_213
, permsAvoiding_231
, permsAvoiding_312
, permsAvoiding_321

  -- * Avoiding two patterns of length 3
, permsAvoiding_123_132
, permsAvoiding_123_132
, permsAvoiding_123_213
, permsAvoiding_123_213
, permsAvoiding_123_312
, permsAvoiding_123_231
, permsAvoiding_123_321
, permsAvoiding_132_213
, permsAvoiding_132_231
, permsAvoiding_132_312
, permsAvoiding_132_321
, permsAvoiding_213_231
, permsAvoiding_213_312
, permsAvoiding_213_321
, permsAvoiding_231_312
, permsAvoiding_231_321
, permsAvoiding_312_321

  -- * Avoiding a pattern of length 4
, permsAvoiding_2413_3142

  -- * Avoiding some patterns
, permsAvoiding
)
where

  import Control.Applicative
  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Tuple    as T

  import qualified Data.Algorithm.PP.Dyck                         as PP.Dyck
  import qualified Data.Algorithm.PP.Perm                         as PP.Perm
  import qualified Data.Algorithm.PP.Perm.Bijection.Trivial       as PP.Perm.Bijection.Trivial
  import qualified Data.Algorithm.PP.Perm.Bijection.SimionSchmidt as PP.Perm.Bijection.SimionSchmidt
  import qualified Data.Algorithm.PP.Perm.Generator.Basic         as PP.Perm.Generator.Basic
  import qualified Data.Algorithm.PP.Perm.Small                   as PP.Perm.Small
  import qualified Data.Algorithm.PP.Utils.Foldable               as PP.Utils.Foldable
  import qualified Data.Algorithm.PP.Utils.List                   as PP.Utils.List

  -- |'permsAvoiding_123' 'n' returns all 123-avoiding permutations of length 'n'.
  --
  -- >>> permsAvoiding_123 3
  -- [[3,2,1],[2,3,1],[3,1,2],[2,1,3],[1,3,2]]
  -- >>> permsAvoiding_123 4
  -- [[4,3,2,1],[3,4,2,1],[4,2,3,1],[3,2,4,1],[2,4,3,1],[4,3,1,2],[3,4,1,2],[4,2,1,3],[4,1,3,2],[3,2,1,4],[2,4,1,3],[3,1,4,2],[2,1,4,3],[1,4,3,2]]
  permsAvoiding_123 :: Int -> [PP.Perm.Perm]
  permsAvoiding_123 = fmap PP.Perm.Bijection.SimionSchmidt.simionSchmidt . permsAvoiding_132

  -- |'permsAvoiding_132' 'n' returns all 132-avoiding permutations of length 'n'.
  --
  -- >>> permsAvoiding_123 3
  -- [[3,2,1],[2,3,1],[3,1,2],[2,1,3],[1,3,2]]
  -- >>> permsAvoiding_123 4
  -- [[4,3,2,1],[3,4,2,1],[4,2,3,1],[3,2,4,1],[2,4,3,1],[4,3,1,2],[3,4,1,2],[4,2,1,3],[4,1,3,2],[3,2,1,4],[2,4,1,3],[3,1,4,2],[2,1,4,3],[1,4,3,2]]
  permsAvoiding_132 :: Int -> [PP.Perm.Perm]
  permsAvoiding_132 = fmap PP.Perm.Bijection.Trivial.rev . permsAvoiding_231

  -- |'permsAvoiding_213' 'n' returns all 213-avoiding permutations of length 'n'.
  --
  -- >>> permsAvoiding_132 3
  -- [[3,2,1],[2,3,1],[3,1,2],[2,1,3],[1,2,3]]
  -- >>> permsAvoiding_132 4
  -- [[4,3,2,1],[3,4,2,1],[4,2,3,1],[3,2,4,1],[2,3,4,1],[4,3,1,2],[3,4,1,2],[4,2,1,3],[4,1,2,3],[3,2,1,4],[2,3,1,4],[3,1,2,4],[2,1,3,4],[1,2,3,4]]
  permsAvoiding_213 :: Int -> [PP.Perm.Perm]
  permsAvoiding_213 = fmap PP.Perm.Bijection.Trivial.comp . permsAvoiding_132

  -- |'permsAvoiding_231' 'n' returns all 231-avoiding permutations of length 'n'.
  --
  -- >>> permsAvoiding_231 3
  -- [[1,2,3],[1,3,2],[2,1,3],[3,1,2],[3,2,1]]
  -- >>> permsAvoiding_231 4
  --[[1,2,3,4],[1,2,4,3],[1,3,2,4],[1,4,2,3],[1,4,3,2],[2,1,3,4],[2,1,4,3],[3,1,2,4],[3,2,1,4],[4,1,2,3],[4,1,3,2],[4,2,1,3],[4,3,1,2],[4,3,2,1]]
  permsAvoiding_231 :: Int -> [PP.Perm.Perm]
  permsAvoiding_231 = fmap (PP.Perm.mkPerm . F.foldr f [] . PP.Dyck.getSteps . PP.Dyck.labelLeftToRightDown) . PP.Dyck.paths
    where
      f (PP.Dyck.LUp i)   acc = i : acc
      f (PP.Dyck.LDown _) acc = acc

  -- |'permsAvoiding_312' 'n' returns all 312-avoiding permutations of length 'n'.
  --
  -- >>> permsAvoiding_312 3
  -- [[1,2,3],[2,1,3],[1,3,2],[2,3,1],[3,2,1]]
  -- >>> permsAvoiding_312 4
  -- [[1,2,3,4],[2,1,3,4],[1,3,2,4],[2,3,1,4],[3,2,1,4],[1,2,4,3],[2,1,4,3],[1,3,4,2],[1,4,3,2],[2,3,4,1],[3,2,4,1],[2,4,3,1],[3,4,2,1],[4,3,2,1]]
  permsAvoiding_312 :: Int -> [PP.Perm.Perm]
  permsAvoiding_312 = fmap PP.Perm.Bijection.Trivial.revComp . permsAvoiding_231

  -- |'permsAvoiding_321' 'n' returns all 321-avoiding permutations of length 'n'.
  --
  -- >>> permsAvoiding_321 3
  -- [[1,2,3],[1,3,2],[2,1,3],[3,1,2],[2,3,1]]
  -- >>> permsAvoiding_321 4
  -- [[1,2,3,4],[1,2,4,3],[1,3,2,4],[1,4,2,3],[1,3,4,2],[2,1,3,4],[2,1,4,3],[3,1,2,4],[2,3,1,4],[4,1,2,3],[3,1,4,2],[2,4,1,3],[3,4,1,2],[2,3,4,1]]
  permsAvoiding_321 :: Int -> [PP.Perm.Perm]
  permsAvoiding_321 = fmap PP.Perm.Bijection.Trivial.rev . permsAvoiding_123

  -- |'permsAvoiding_123_132' 'n' returns all (123, 132)-avoiding permutations of length 'n'.
  -- class C.
  --
  -- >>> permsAvoiding_123_132 4
  -- [[3,4,2,1],[3,4,1,2],[3,2,4,1],[3,2,1,4],[4,3,2,1],[4,3,1,2],[4,2,3,1],[4,2,1,3]]
  permsAvoiding_123_132 :: Int -> [PP.Perm.Perm]
  permsAvoiding_123_132 n
    | n <= 0    = []
    | n == 1    = PP.Perm.Generator.Basic.perms 1
    | n == 2    = PP.Perm.Generator.Basic.perms 2
    | otherwise = (PP.Perm.mkPerm . L.reverse) <$> aux (n-2) [[n, n-1], [n-1, n]]
      where
        aux 0 xss = xss
        aux k xss = aux (k-1) $ concatMap (\ (x : xs) -> [k : x : xs, x : k : xs]) xss

  -- |'permsAvoiding_123_213' 'n' returns all (123, 213)-avoiding permutations of length 'n'.
  -- class C.
  --
  -- >>> permsAvoiding_123_213 4
  -- [[4,3,1,2],[3,4,1,2],[4,1,3,2],[1,4,3,2],[4,3,2,1],[3,4,2,1],[4,2,3,1],[2,4,3,1]]
  permsAvoiding_123_213 :: Int -> [PP.Perm.Perm]
  permsAvoiding_123_213 = fmap PP.Perm.Bijection.Trivial.revComp . permsAvoiding_123_132

  -- |'permsAvoiding_123_312' 'n' returns all (123, 312)-avoiding permutations of length 'n'.
  -- Class D.
  --
  -- >>> permsAvoiding_123_312 4
  -- [[1,2,4,3],[2,1,4,3],[1,4,3,2],[4,3,2,1],[1,2,3,4],[2,1,3,4],[1,3,2,4],[3,2,1,4]]
  permsAvoiding_123_312 :: Int -> [PP.Perm.Perm]
  permsAvoiding_123_312 = fmap PP.Perm.Bijection.Trivial.comp . permsAvoiding_132_321

  -- |'permsAvoiding_123_231' 'n' returns all (123, 231)-avoiding permutations of length 'n'.
  -- Class D.
  --
  -- >>> permsAvoiding_123_231 4
  -- [[2,1,3,4],[2,1,4,3],[3,2,1,4],[4,3,2,1],[1,2,3,4],[1,2,4,3],[1,3,2,4],[1,4,3,2]]
  permsAvoiding_123_231 :: Int -> [PP.Perm.Perm]
  permsAvoiding_123_231 = fmap PP.Perm.Bijection.Trivial.rev . permsAvoiding_132_321

  -- |'permsAvoiding_123_321' 'n' returns all (123, 321)-avoiding permutations of length 'n'.
  -- There is no such permutation for @n>4@.
  -- Class E.
  --
  -- >>> permsAvoiding_123_321 3
  -- [[1,3,2],[2,1,3],[2,3,1],[3,1,2]]
  -- >>> permsAvoiding_123_321 4
  -- [[3,4,1,2],[2,4,1,3],[3,1,4,2],[2,1,4,3]]
  -- >>> permsAvoiding_123_321 5
  -- []
  permsAvoiding_123_321 :: Int -> [PP.Perm.Perm]
  permsAvoiding_123_321 n
    | n <= 0    = []
    | n == 1    = PP.Perm.Generator.Basic.perms 1
    | n == 2    = PP.Perm.Generator.Basic.perms 2
    | n == 3    = [PP.Perm.Small.p_132, PP.Perm.Small.p_213, PP.Perm.Small.p_231, PP.Perm.Small.p_312]
    | n == 4    = [PP.Perm.Small.p_3412, PP.Perm.Small.p_2413, PP.Perm.Small.p_3142, PP.Perm.Small.p_2143]
    | otherwise = []

  -- |'permsAvoiding_132_213' 'n' returns all (132, 213)-avoiding permutations of length 'n'.
  -- class B.
  --
  -- >>> permsAvoiding_132_213 4
  -- [[4,3,1,2],[3,4,1,2],[4,1,2,3],[1,2,3,4],[4,3,2,1],[3,4,2,1],[4,2,3,1],[2,3,4,1]]
  permsAvoiding_132_213 :: Int -> [PP.Perm.Perm]
  permsAvoiding_132_213 n
    | n <= 0    = []
    | n == 1    = PP.Perm.Generator.Basic.perms 1
    | n == 2    = PP.Perm.Generator.Basic.perms 2
    | otherwise = PP.Perm.mkPerm <$> aux 3 [[1, 2], [2, 1]]
      where
        aux k xss
          | k > n     = xss
          | otherwise = aux (k+1) $ concatMap (\ xs -> [k : xs, PP.Utils.List.insertAfterMax k xs]) xss

  -- |'permsAvoiding_132_231' 'n' returns all (132, 231)-avoiding permutations of length 'n'.
  -- Class A.
  --
  -- >>> permsAvoiding_132_231 4
  -- [[4,3,2,1],[3,4,2,1],[2,4,3,1],[2,3,4,1],[1,4,3,2],[1,3,4,2],[1,2,4,3],[1,2,3,4]]
  permsAvoiding_132_231 :: Int -> [PP.Perm.Perm]
  permsAvoiding_132_231 = fmap PP.Perm.Bijection.Trivial.invComp . permsAvoiding_213_231

  -- |'permsAvoiding_132_312' 'n' returns all (132, 312)-avoiding permutations of length 'n'.
  -- Class A.
  --
  -- >>> permsAvoiding_132_312 4
  -- [[4,3,2,1],[3,4,2,1],[3,2,4,1],[2,3,4,1],[3,2,1,4],[2,3,1,4],[2,1,3,4],[1,2,3,4]]
  permsAvoiding_132_312 :: Int -> [PP.Perm.Perm]
  permsAvoiding_132_312 = fmap PP.Perm.Bijection.Trivial.rev . permsAvoiding_213_231

  -- |'permsAvoiding_132_321' 'n' returns all (132, 321)-avoiding permutations of length 'n'.
  -- Class D.
  --
  -- >>> permsAvoiding_132_321 4
  -- [[4,3,1,2],[3,4,1,2],[4,1,2,3],[1,2,3,4],[4,3,2,1],[3,4,2,1],[4,2,3,1],[2,3,4,1]]
  permsAvoiding_132_321 :: Int -> [PP.Perm.Perm]
  permsAvoiding_132_321 n
    | n <= 0    = []
    | n == 1    = PP.Perm.Generator.Basic.perms 1
    | n == 2    = PP.Perm.Generator.Basic.perms 2
    | otherwise = PP.Perm.mkPerm <$> aux 3 [[1, 2], [2, 1]]
      where
        aux k xss
          | k > n     = xss
          | otherwise = aux (k+1) $ concatMap f xss
            where
              f xs
                | xs == [1,2..(k-1)] = [k : xs, xs ++ [k]]
                | otherwise          = L.nub [k : xs, PP.Utils.List.insertAfterMax k xs]

  -- |'permsAvoiding_213_231' 'n' returns all (213, 231)-avoiding permutations of length 'n'.
  -- Class A.
  --
  -- >>> permsAvoiding_213_231 4
  -- [[1,2,3,4],[1,2,4,3],[1,4,2,3],[1,4,3,2],[4,1,2,3],[4,1,3,2],[4,3,1,2],[4,3,2,1]]
  permsAvoiding_213_231 :: Int -> [PP.Perm.Perm]
  permsAvoiding_213_231 n = PP.Perm.mkPerm <$> aux [1..n]
    where
      aux []  = [[]]
      aux [x] = [[x]]
      aux xs  = xss' ++ xss''
        where
          xss'  = fmap (L.head xs :) . aux $ L.tail xs
          xss'' = fmap (L.last xs :) . aux $ L.init xs

  -- |'permsAvoiding_213_312' 'n' returns all (213, 312)-avoiding permutations of length 'n'.
  -- Class A.
  --
  -- >>> permsAvoiding_213_312 4
  -- [[1,2,3,4],[1,2,4,3],[1,3,4,2],[1,4,3,2],[2,3,4,1],[2,4,3,1],[3,4,2,1],[4,3,2,1]]
  permsAvoiding_213_312 :: Int -> [PP.Perm.Perm]
  permsAvoiding_213_312 = fmap PP.Perm.Bijection.Trivial.inv . permsAvoiding_213_231

  -- |'permsAvoiding_213_321' 'n' returns all (213, 321)-avoiding permutations of length 'n'.
  -- Class D.
  --
  -- >>> permsAvoiding_213_321 4
  -- [[3,4,2,1],[3,4,1,2],[2,3,4,1],[1,2,3,4],[4,3,2,1],[4,3,1,2],[4,2,3,1],[4,1,2,3]]
  permsAvoiding_213_321 :: Int -> [PP.Perm.Perm]
  permsAvoiding_213_321 = fmap PP.Perm.Bijection.Trivial.revComp . permsAvoiding_132_321

  -- |'permsAvoiding_231_312' 'n' returns all (231, 312)-avoiding permutations of length 'n'.
  -- class B.
  --
  -- >>> permsAvoiding_231_312 4
  -- [[2,1,3,4],[2,1,4,3],[3,2,1,4],[4,3,2,1],[1,2,3,4],[1,2,4,3],[1,3,2,4],[1,4,3,2]]
  permsAvoiding_231_312 :: Int -> [PP.Perm.Perm]
  permsAvoiding_231_312 = fmap PP.Perm.Bijection.Trivial.rev . permsAvoiding_132_213

  -- |'permsAvoiding_231_321' 'n' returns all (231, 321)-avoiding permutations of length 'n'.
  -- class C.
  --
  -- >>> permsAvoiding_231_321 4
  -- [[4,3,1,2],[3,4,1,2],[4,2,1,3],[3,2,1,4],[4,3,2,1],[3,4,2,1],[4,2,3,1],[3,2,4,1]]
  permsAvoiding_231_321 :: Int -> [PP.Perm.Perm]
  permsAvoiding_231_321 = fmap PP.Perm.Bijection.Trivial.inv . permsAvoiding_123_132

  -- |'permsAvoiding_312_321' 'n' returns all (312, 321)-avoiding permutations of length 'n'.
  -- class C.
  --
  -- >>> permsAvoiding_312_321 4
  -- [[2,1,3,4],[2,1,4,3],[2,3,1,4],[2,3,4,1],[1,2,3,4],[1,2,4,3],[1,3,2,4],[1,3,4,2]]
  permsAvoiding_312_321 :: Int -> [PP.Perm.Perm]
  permsAvoiding_312_321 = fmap PP.Perm.Bijection.Trivial.comp . permsAvoiding_123_132

  -- |'permsAvoiding_2413_3142' 'n' returns all (2413, 3142)-avoiding permutations of length 'n'
  -- (aka separable permutations).
  permsAvoiding_2413_3142 :: Int -> [PP.Perm.Perm]
  permsAvoiding_2413_3142 n = []


  -- |'permsAvoiding' 'n' 'ps' returns all permutations of length 'n' that avoid the
  -- permutations of 'ps'.
  permsAvoiding :: Int -> [PP.Perm.Perm] -> [PP.Perm.Perm]
  permsAvoiding _ _ = []
  --permsAvoiding n = F.foldr (L.filter (PP.Perm.Avoidance.avoid p)) (PP.Perm.Generator.Basic.perms n) . L.sort . PP.Utils.List.uniq
