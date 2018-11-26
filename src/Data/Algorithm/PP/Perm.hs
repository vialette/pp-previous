module Data.Algorithm.PP.Perm
(
  -- * Types
  X
, Y
, Perm
, FPerm
, Pattern

  -- * Making
, identity
, fromList
, mk

  -- * Transforming
, toList
, toPoints
, toPoints'

  -- * Generating
, perms

  -- Querying
, len
, at

-- * Trivial bijections
, inv
, rev
, comp

-- * Composing trivial bijections
, revComp
, compInv
, invComp
, invRevComp

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

  import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point
  import qualified Data.Algorithm.PP.Combi          as PP.Combi
  import qualified Data.Algorithm.PP.Utils.List     as PP.Utils.List

  -- |
  type X = Int

  -- |
  type Y = Int

  -- |'Perm' type
  newtype Perm = PermImpl { getElems :: [Y] } deriving (Eq, Ord)

  -- |
  type FPerm = Perm -> Perm

  -- |
  type Pattern = [Y]

  -- |
  instance Show Perm where
    show = show . getElems

  -- |'fromList' 'xs' makes a permutation from the list 'xs'.
  -- Duplicate elements in 'xs' are ordered from left to right.
  --
  -- >>> fromList "acba"
  -- [1,4,3,2]
  -- >>> fromList [2,9,7,2]
  -- [1,4,3,2]
  fromList :: (Ord a) => [a] -> Perm
  fromList = PermImpl . reduce

  -- | Alias for 'fromList'.
  mk :: (Ord a) => [a] -> Perm
  mk = fromList

  -- | 'toList' 'p' returns the list of the elements of permutation 'p'.
  toList :: Perm -> [Y]
  toList = getElems

  -- | 'toPoints' 'p' returns the permutation 'p' as points.
  --
  -- >>> toPoints (mk [1,3,4,2])
  -- [(1,1),(2,3),(3,4),(4,2)]
  --- >>> :type toPoints (mk [1,3,4,2])
  -- toPoints (mk [1,3,4,2]) :: [Data.Algorithm.PP.Geometry.Point.Point]
  toPoints :: Perm -> [PP.Geometry.Point.Point]
  toPoints = L.zipWith PP.Geometry.Point.mk [1..] . toList

  -- | 'toPoints'' 'p' returns the permutation 'p' as pairs of integers.
  --
  -- >>> toPoints' (mk [1,3,4,2])
  -- [(1,1),(2,3),(3,4),(4,2)]
  --- >>> :type toPoints' (mk [1,3,4,2])
  -- toPoints' (mk [1,3,4,2]) :: [(Data.Algorithm.PP.Perm.X, Data.Algorithm.PP.Perm.Y)]
  toPoints' :: Perm -> [(X, Y)]
  toPoints' = L.map PP.Geometry.Point.getCoordinates . toPoints

  -- Reduce a list of elements.
  reduce :: (Ord a) => [a] -> [Y]
  reduce = L.map T.fst . L.sortBy cmpFstSnd . L.zip [1..] . L.sortBy cmpSnd . L.zip [1..]
    where
      cmpFstSnd = compare `on` (T.fst . T.snd)
      cmpSnd    = compare `on` T.snd

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
  len = L.length . toList

  -- | 'p `at` i' returns the element at position 'i' in permutation 'p'.
  at :: Perm -> Int -> Y
  at p i = xs L.!! i
    where
      xs = toList p

  -- | 'inv p' returns the inverse of permutation 'p'.
  --
  -- >>> inv $ mk [1,3,4,2]
  -- [1,4,2,3]
  inv :: Perm -> Perm
  inv = PermImpl . L.map T.snd . L.sort . flip L.zip [1..] . toList

  -- | 'rev' 'p' returns the reverse of permutation 'p'.
  --
  -- >>> rev $ mk [1,3,4,2]
  -- [2,4,3,1]
  rev :: Perm -> Perm
  rev = PermImpl . L.reverse . toList

  -- | 'comp' 'p' returns the complement of permutation 'p'.
  --
  -- >>> comp $ mk [1,3,4,2]
  -- [4,2,1,3]
  comp :: Perm -> Perm
  comp p = PermImpl $ fmap (\y -> m - y + 1) ys
    where
      ys = toList p
      m  = F.maximum ys

  -- | 'revComp' 'p' returns the reverse complement of permutation 'p'.
  --
  -- >>> revComp $ mk [1,3,4,2]
  -- [3,1,2,4]
  revComp :: Perm -> Perm
  revComp = rev . comp

  -- | 'compInv' 'p' returns the complement inverse of permutation 'p'.
  --
  -- >>> compInv $ mk [1,3,4,2]
  -- [4,1,3,2]
  compInv :: Perm -> Perm
  compInv = comp . inv

  -- | 'invComp' 'p' returns the inverse complement of permutation 'p'.
  --
  -- >>> invComp $ mk [1,3,4,2]
  -- [3,2,4,1]
  invComp :: Perm -> Perm
  invComp = inv . comp

  -- | 'invRevComp' 'p' returns the inverse reverse complement of permutation 'p'.
  --
  -- >>> invRevComp $ mk [1,3,4,2]
  -- [2,3,1,4
  invRevComp :: Perm -> Perm
  invRevComp = inv . rev . comp

  -- transpose :: Int -> Int -> Perm -> Perm
  -- transpose i j = mk $ L.prefix (i-1) xs ++ [xs L.!! i] ++

  -- |'extendLeft' 'p' takes a permutation 'p' of length 'n' and returns all
  -- permutations of length 'n'+1 which suffix of length 'n' is order-isomorphic
  -- to 'p'.
  --
  -- >>> extendLeft $ mk [2,1,3]
  -- [[1,3,2,4],[2,3,1,4],[3,2,1,4],[4,2,1,3]]
  extendLeft :: Perm -> [Perm]
  extendLeft p = L.map mk [k : f k ys | k <- [1..len p+1]]
    where
      ys  = toList p
      f k = F.foldr (\y acc -> (if y<k then y else y+1) : acc) []

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
      select ys = if L.null ys then [] else L.head ys

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
  inversions :: Perm -> [(Y, Y)]
  inversions = L.map (\[i, j] -> (i, j)) . L.filter (\[i, j] -> i > j) . PP.Combi.subsets 2 . toList
