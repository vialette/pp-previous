module Data.Algorithm.PP.Perm.Generator.Basic
(
  perms
, lexPerms

, derangements

, extendLeft
, extendRight
)
where

  import qualified Data.Foldable as F
  import qualified Data.List     as L

  import qualified Data.Algorithm.PP.Perm                   as PP.Perm
  import qualified Data.Algorithm.PP.Perm.Bijection.Trivial as PP.Perm.Bijection.Trivial
  import qualified Data.Algorithm.PP.Perm.Prop              as PP.Perm.Prop

  -- | 'perms' 'n' returns all permutations of length 'n'.
  --
  -- >>> perms 0
  -- [[]]
  -- >>> perms 1
  -- [[1]]
  -- >>> perms 2
  -- [[1,2],[2,1]]
  -- >>> perms 3
  -- [[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]
  perms :: Int -> [PP.Perm.Perm]
  perms n = L.map PP.Perm.mkPermUnsafe $ L.permutations [1..n]

  lexPerms :: Int ->[PP.Perm.Perm]
  lexPerms _ = []

  -- |'derangements' 'n' returns all derangements of length 'n'.
  derangements :: Int -> [PP.Perm.Perm]
  derangements = L.filter PP.Perm.Prop.derangement . perms

  -- transpose :: Int -> Int -> Perm -> Perm
  -- transpose i j = mk $ L.prefix (i-1) xs ++ [xs L.!! i] ++

  -- |'extendLeft' 'p' takes a permutation 'p' of length 'n' and returns all
  -- permutations of length 'n'+1 which suffix of length 'n' is order-isomorphic
  -- to 'p'.
  --
  -- >>> extendLeft $ mk [2,1,3]
  -- [[1,3,2,4],[2,3,1,4],[3,2,1,4],[4,2,1,3]]
  extendLeft :: PP.Perm.Perm -> [PP.Perm.Perm]
  extendLeft p = L.map PP.Perm.mkPermUnsafe [k : f k ys | k <- [1..PP.Perm.len p+1]]
    where
      ys  = PP.Perm.getList p
      f k = F.foldr (\y acc -> (if y < k then y else y+1) : acc) []

  -- |'extendRight' 'p' takes a permutation 'p' of length 'n' and returns all
  -- permutations of length 'n'+1 which prefix of length 'n' is order-isomorphic
  -- to 'p'.
  --
  -- >>> extendRight $ mk [2,1,3]
  -- [[3,2,4,1],[3,1,4,2],[2,1,4,3],[2,1,3,4]]
  extendRight :: PP.Perm.Perm -> [PP.Perm.Perm]
  extendRight = L.map PP.Perm.Bijection.Trivial.rev . extendLeft . PP.Perm.Bijection.Trivial.rev
