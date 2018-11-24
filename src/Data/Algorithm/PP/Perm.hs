module Data.Algorithm.PP.Perm
(
  T
, Perm
, FPerm

, identity
, fromList
, mk

, toList

, perms

, len
, at

, module Data.Algorithm.PP.Perm.Bijection

, transpose

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

, shuffle
, shuffle2
, shuffle3
, shuffle4
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

  -- | 'len p' returns the length of permutation 'p'.
  len :: Perm -> Int
  len = L.length . getElems

  -- | 'p `at` i' returns the element at position 'i' in permutation 'p'.
  at :: Perm -> Int -> T
  at p i = xs L.!! i
    where
      xs = getElems p

  transpose :: Int -> Int -> Perm -> Perm
  transpose i j = mk $ L.prefix (i-1) xs ++ [xs L.!! i] ++ 

  extendLeft :: Perm -> [Perm]
  extendLeft p = L.map mk [k : f k xs | k <- [1..len p+1]]
    where
      xs  = toList p
      f k = F.foldr (\x acc -> (if x<k then x else x+1) : acc) []

  extendRight :: Perm -> [Perm]
  extendRight = L.map rev . extendLeft . rev

  prefix :: Int -> Perm -> Perm
  prefix k = mk . L.take k . toList

  prefixes :: Perm -> [Perm]
  prefixes = L.map mk . L.tail . L.inits . toList

  suffix :: Int -> Perm -> Perm
  suffix k = mk . L.take k . toList

  suffixes :: Perm -> [Perm]
  suffixes = L.map mk . L.init . L.tails . toList

  -- | 'sub' 'k' 'p' returns all distinct permutations of length 'k' that occurs in
  -- permutation 'p'.
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
  factors :: Int -> Perm -> [Pattern]
  factors k = PP.Utils.List.uniq . PP.Utils.List.chunk k . toList

  permFactors :: Int -> Perm -> [Perm]
  permFactors k = L.map mk . factors k

  maxPermFactors :: (Perm -> Bool) -> Perm -> [Perm]
  maxPermFactors f p = select $ L.dropWhile L.null [[q | q <- permFactors k p, f q] | k <- [n,n-1..1]]
    where
      n         = len p
      select xs = if L.null xs then [] else L.head xs


  -- |'shuffle2' 'p' 'q' return all distinct permutations that can be be obtained by
  -- shuffling permutation 'p' and 'q'.
  --
  -- >>>  shuffle2 (mk [1,2]) (mk [2,1])
  -- [[1,3,4,2],[1,3,4,2],[1,3,2,4],[3,1,4,2],[3,1,2,4],[3,1,2,4]]
  shuffle2 :: Perm -> Perm -> [Perm]
  shuffle2 p q = shuffle [p, q]

  -- |'shuffle3' 'p' 'q' 'r' return all distinct permutations that can be be obtained by
  -- shuffling permutation 'p', 'q' and 'r'.
  shuffle3 :: Perm -> Perm -> Perm -> [Perm]
  shuffle3 p q r = shuffle [p, q, r]

  -- |'shuffle4' 'p' 'q' 'r' 's' return all distinct permutations that can be be obtained by
  -- shuffling permutation 'p', 'q', 'r' and 's'.
  shuffle4 :: Perm -> Perm -> Perm -> Perm -> [Perm]
  shuffle4 p q r s = shuffle [p, q, r, s]

  -- |'shuffle' 'ps' return all distinct permutations that can be be obtained by
  -- shuffling permutations in 'ps'.
  --
  -- >>> shuffle [mk [1,2], mk [2,1]]
  -- [[1,3,4,2],[1,3,4,2],[1,3,2,4],[3,1,4,2],[3,1,2,4],[3,1,2,4]]
  shuffle :: [Perm] -> [Perm]
  shuffle = L.map mk . PP.Utils.List.shuffle . L.map toList
