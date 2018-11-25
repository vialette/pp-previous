module Data.Algorithm.PP.Perm
(
  T
, Perm
, FPerm
, Pattern

, identity
, fromList
, mk

, toList

, perms

, len
, at

, module Data.Algorithm.PP.Perm.Bijection

-- , transpose

, extendLeft
, extendRight

, prefix
, prefixes
, suffix
, suffixes

, patterns
, permPatterns
, maxPermPatterns

, factors
, permFactors
, maxPermFactors

, inversions
)
where

  import qualified Data.List      as L
  import qualified Data.Foldable  as F
  import qualified Data.Tuple     as T
  import Data.Function (on)

  import Data.Algorithm.PP.Perm.Inner
  import Data.Algorithm.PP.Perm.Bijection

  import qualified Data.Algorithm.PP.Combi      as PP.Combi
  import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List

  -- | 'identity' 'n' returns the identity permutation of length 'n'.
  --
  -- >>> identity 4
  -- [1,2,3,4]
  identity :: Int -> Perm
  identity n = PermImpl [1 .. n]

  -- | Rturn the empty permutation.
  empty :: Perm
  empty = PermImpl []

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
  perms :: Int -> [Perm]
  perms n = L.map PermImpl $ L.permutations [1..n]

  -- | 'len' 'p' returns the length of the permutation 'p'.
  len :: Perm -> Int
  len = L.length . getElems

  -- | 'p `at` i' returns the element at position 'i' in permutation 'p'.
  at :: Perm -> Int -> T
  at p i = xs L.!! i
    where
      xs = getElems p

  -- transpose :: Int -> Int -> Perm -> Perm
  -- transpose i j = mk $ L.prefix (i-1) xs ++ [xs L.!! i] ++

  -- |'extendLeft' 'p' takes a permutation 'p' of length 'n' and returns all
  -- permutations of length 'n'+1 which suffix of length 'n' is order-isomorphic
  -- to 'p'.
  --
  -- >>> extendLeft $ mk [2,1,3]
  -- [[1,3,2,4],[2,3,1,4],[3,2,1,4],[4,2,1,3]]
  extendLeft :: Perm -> [Perm]
  extendLeft p = L.map mk [k : f k xs | k <- [1..len p+1]]
    where
      xs  = toList p
      f k = F.foldr (\x acc -> (if x<k then x else x+1) : acc) []

  -- |'extendRight' 'p' takes a permutation 'p' of length 'n' and returns all
  -- permutations of length 'n'+1 which prefix of length 'n' is order-isomorphic
  -- to 'p'.
  --
  -- >>> extendRight $ mk [2,1,3]
  -- [[3,2,4,1],[3,1,4,2],[2,1,4,3],[2,1,3,4]]
  extendRight :: Perm -> [Perm]
  extendRight = L.map rev . extendLeft . rev

  -- 'prefix' 'k' 'p' returns the prefix of length 'k' of the permutation 'p' as
  -- a permutation.
  --
  -- >>> p = mk [2,4,5,1,6,3]
  -- >>> [prefix i p | i <- [1..len p]]
  -- [[1],[1,2],[1,2,3],[2,3,4,1],[2,3,4,1,5],[2,4,5,1,6,3]]
  prefix :: Int -> Perm -> Perm
  prefix k = mk . L.take k . toList

  -- |'prefixes' 'p' returns all the prefixes of the permutation 'p' as
  -- permutations.
  --
  -- >>> p = mk [2,4,5,1,6,3]
  -- >>> prefixes p
  -- [[1],[1,2],[1,2,3],[2,3,4,1],[2,3,4,1,5],[2,4,5,1,6,3]]
  prefixes :: Perm -> [Perm]
  prefixes = L.map mk . L.tail . L.inits . toList

  -- 'suffix' 'k' 'p' returns the suffix of length 'k' of the permutation 'p' as
  -- a permutation.
  --
  -- >>> p = mk [2,4,5,1,6,3]
  -- >>> [suffix i p | i <- [1..len p]]
  -- [[1],[2,1],[1,3,2],[3,1,4,2],[3,4,1,5,2],[2,4,5,1,6,3]]
  suffix :: Int -> Perm -> Perm
  suffix k p = mk . L.drop (n-k) $ toList p
    where
      n = len p

  -- |'suffixes' 'p' returns all the suffixes of the permutation 'p' as
  -- permutations.
  --
  -- >>> p = mk [2,4,5,1,6,3]
  -- >>> suffixes p
  -- [[2,4,5,1,6,3],[3,4,1,5,2],[3,1,4,2],[1,3,2],[2,1],[1]]
  suffixes :: Perm -> [Perm]
  suffixes = L.map mk . L.init . L.tails . toList

  -- | 'patterns' 'k' 'p' returns all distinct permutations of length 'k' that
  -- occurs in permutation 'p'.
  --
  -- >>> patterns 0 (mk [2,4,1,3,5])
  -- [[]]
  -- >>> patterns 1 (mk [2,4,1,3,5])
  -- [[1]]
  -- >>> patterns 2 (mk [2,4,1,3,5])
  -- [[1,2],[2,1]]
  -- >>> patterns 3 (mk [2,4,1,3,5])
  -- [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2]]
  -- >>> patterns 4 (mk [2,4,1,3,5])
  -- [[1,3,2,4],[2,1,3,4],[2,3,1,4],[2,4,1,3],[3,1,2,4]]
  -- >>> patterns 5 (mk [2,4,1,3,5])
  -- [[2,4,1,3,5]]
  -- >>> patterns 6 (mk [2,4,1,3,5])
  -- []
  patterns :: Int -> Perm -> [Pattern]
  patterns k = PP.Utils.List.uniq . PP.Combi.subsets k . toList

  permPatterns :: Int -> Perm -> [Perm]
  permPatterns k = L.map mk . patterns k

  -- |'maxPatterns' 'f' 'p' returns the longest patterns 'q' of permutation 'p'
  -- such that 'f' 'q' holds.
  maxPermPatterns :: (Perm -> Bool) -> Perm -> [Perm]
  maxPermPatterns f p = select $ L.dropWhile L.null [[q | q <- permPatterns k p, f q] | k <- [n,n-1..1]]
    where
      n         = len p
      select xs = if L.null xs then [] else L.head xs

  -- |'factors' 'k' 'p' returns the list of all factors of length 'k' of the
  -- permutation 'p'.
  --
  -- >>> p = mk [2,4,1,3]
  -- >>> factors 0 p
  -- []
  -- >>> factors 1 p
  -- [[1],[2],[3],[4]]
  -- >>> factors 2 p
  -- [[1,3],[2,4],[4,1]]
  -- >>> factors 3 p
  -- [[2,4,1],[4,1,3]]
  -- >>> factors 4 p
  -- [[2,4,1,3]]
  -- >>> factors 5 p
  -- []
  factors :: Int -> Perm -> [Pattern]
  factors k = PP.Utils.List.uniq . PP.Utils.List.chunk k . toList

  -- |'permFactors' 'k' 'p' returns the list of all permutations of length 'k'
  -- that occur as a factor in the permutation 'p'.
  -- permutation 'p'.
  --
  -- >>> p = mk [2,4,1,3]
  -- >>> permFactors 0 p
  -- []
  -- >>> permFactors 1 p
  -- [[1],[1],[1],[1]]
  -- >>> permFactors 2 p
  -- [[1,2],[1,2],[2,1]]
  -- >>> permFactors 3 p
  -- [[2,3,1],[3,1,2]]
  -- >>> permFactors 4 p
  -- [[2,4,1,3]]
  -- >>> permFactors 5 p
  -- []
  permFactors :: Int -> Perm -> [Perm]
  permFactors k = L.map mk . factors k

  maxPermFactors :: (Perm -> Bool) -> Perm -> [Perm]
  maxPermFactors f p = select $ L.dropWhile L.null [[q | q <- permFactors k p, f q] | k <- [n,n-1..1]]
    where
      n         = len p
      select xs = if L.null xs then [] else L.head xs



  -- |'inversions' 'p' returns the inversions of the permutation 'p'.
  --
  -- >>> inversions (mk [1,5,3,2,6,4])
  -- [(5,3),(5,2),(5,4),(3,2),(6,4)]
  inversions :: Perm -> [(T, T)]
  inversions = L.map (\[i, j] -> (i, j)) . L.filter (\[i, j] -> i > j) . PP.Combi.subsets 2 . toList
