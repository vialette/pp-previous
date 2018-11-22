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
, module Data.Algorithm.PP.Pattern

, patterns
, maxPattern
, maxPattern'
, maxPatterns

, factors

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
  patterns :: Int -> Perm -> [Perm]
  patterns k = L.map mk . PP.Utils.List.uniq . PP.Combi.subsets k . toList

  -- |'factors' 'k' 'p' returns the list of all factors of length 'k' of the
  -- permutation 'p'.
  factors :: Int -> Perm -> [Pattern]
  factors k = PP.Utils.List.uniq . PP.Utils.List.chunk k . toList

  factors' :: Int -> Perm -> [Perm]
  factors' k = L.map mk . factors

  maxPatternsAux :: (Perm -> Bool) -> Perm -> [[Perm]]
  maxPatternsAux f p = L.dropWhile L.null [[q | q <- patterns k p, f q] | k <- [n,n-1..1]]
    where
      n  = len p

  -- |'maxPatterns' 'f' 'p' returns the longest patterns 'q' of permutation 'p'
  -- such that 'f' 'q' holds.
  maxPatterns :: (Perm -> Bool) -> Perm -> [Perm]
  maxPatterns f p = case maxPatternsAux f p of
                    []       -> []
                    (qs : _) -> qs

  -- |'maxPattern' 'f' 'p' returns a longest pattern 'q' of permutation 'p'
  -- such that 'f' 'q' holds.
  maxPattern :: (Perm -> Bool) -> Perm -> Maybe Perm
  maxPattern f = PP.Utils.List.safeHead . maxPatterns f

  -- |
  maxPattern' :: (Perm -> Bool) -> Perm -> Perm
  maxPattern' f = L.head . maxPatterns f

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
