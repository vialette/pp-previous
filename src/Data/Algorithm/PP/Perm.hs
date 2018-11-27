module Data.Algorithm.PP.Perm
(
  -- * Types
  Perm
, FPerm
, Pattern

  -- * Points
, module Data.Algorithm.PP.Geometry.Point

  -- * Making
, identity
, fromList
, mk

  -- * Transforming
, toList
, getPoints
, coords

  -- * Generating
, perms

  -- Querying
, len

-- * Trivial bijections
, inv
, rev
, comp

-- * Composing trivial bijections
, revComp
, compRev
, invRev
, compInv
, invComp
, revInv
, invRevComp

-- , transpose

, extendLeft
, extendRight

, prefix
, prefixes
, suffix
, suffixes

, patterns
, patterns'
, maxpatterns'

, factors
, factors'
, maxfactors'

, inversions

  -- * Displaying
, grid
)
where

  import qualified Data.List      as L
  import qualified Data.Foldable  as F
  import qualified Data.Tuple     as T
  import Data.Function (on)

  import Data.Algorithm.PP.Geometry.Point (X, Y, Point)
  import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point
  import qualified Data.Algorithm.PP.Combi          as PP.Combi
  import qualified Data.Algorithm.PP.Utils.List     as PP.Utils.List

  -- |'Perm' type
  newtype Perm = Perm { getPoints :: [Point] } deriving (Eq, Ord)

  -- |
  type Pattern = Perm

  -- |
  type FPerm = Perm -> Perm

  -- |
  instance Show Perm where
    show = show . L.map PP.Geometry.Point.getY . getPoints

  -- |'fromList' 'xs'
  fromList :: (Ord a) => [a] -> Perm
  fromList = mk

  -- | 'mk' 'xs'
  mk :: (Foldable t, Ord a) => t a -> Perm
  mk = Perm . PP.Geometry.Point.mks [1..] . reduce . F.toList

  fromListUnsafe :: [Y] -> Perm
  fromListUnsafe = fromPointsUnsafe . PP.Geometry.Point.mks [1..]

  -- Used to convert a lists of points to a pattern.
  fromPointsUnsafe :: [Point] -> Pattern
  fromPointsUnsafe = Perm

  -- | 'toList' 'p' returns the list of the elements of permutation 'p'.
  toList :: Perm -> [Y]
  toList = L.map PP.Geometry.Point.getY . getPoints

  -- | 'coords' 'p' returns the permutation 'p' as pairs of integers.
  --
  -- >>> coords (mk [1,3,4,2])
  -- [(1,1),(2,3),(3,4),(4,2)]
  --- >>> :type coords (mk [1,3,4,2])
  -- coords (mk [1,3,4,2]) :: [(Data.Algorithm.PP.Perm.X, Data.Algorithm.PP.Perm.Y)]
  coords :: Perm -> [(X, Y)]
  coords = L.map PP.Geometry.Point.getCoords . getPoints

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
  identity n = fromPointsUnsafe $ PP.Geometry.Point.mks [1..n] [1..n]

  -- | Rturn the empty permutation.
  empty :: Perm
  empty = Perm []

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
  perms n = L.map fromListUnsafe $ L.permutations [1..n]

  -- | 'len' 'p' returns the length of the permutation 'p'.
  len :: Perm -> Int
  len = L.length . getPoints

  -- | 'inv' 'p' returns the inverse of the permutation 'p'.
  --
  -- prop> inv (inv p) = p
  --
  -- >>> inv $ mk [1,3,4,2]
  -- [1,4,2,3]
  inv :: Perm -> Perm
  inv = fromListUnsafe . L.map PP.Geometry.Point.getX . L.sortBy PP.Geometry.Point.cmpX . L.map PP.Geometry.Point.symm . getPoints

  -- | 'rev' 'p' returns the reverse of the permutation 'p'.
  --
  -- prop> rev (rev p) = p
  --
  -- >>> rev $ mk [1,3,4,2]
  -- [2,4,3,1]
  rev :: Perm -> Perm
  rev = fromListUnsafe . L.reverse . toList

  -- | 'comp' 'p' returns the complement of the permutation 'p'.
  --
  -- prop> comp (comp p) = p
  --
  -- >>> comp $ mk [1,3,4,2]
  -- [4,2,1,3]
  comp :: Perm -> Perm
  comp p = fromListUnsafe $ fmap (\y -> m-y+1) ys
    where
      ys = toList p
      m  = F.maximum ys

  -- | 'revComp' 'p' returns the reverse complement of the permutation 'p'.
  --
  -- >>> revComp $ mk [1,3,4,2]
  -- [3,1,2,4]
  revComp :: Perm -> Perm
  revComp = rev . comp

  -- | 'compRev' 'p' returns the complement reverse of the permutation 'p'.
  --
  -- prop> revComp p == compRev p
  compRev :: Perm -> Perm
  compRev = revComp

  -- | 'compInv' 'p' returns the complement inverse of the permutation 'p'.
  --
  -- >>> compInv $ mk [1,3,4,2]
  -- [4,1,3,2]
  compInv :: Perm -> Perm
  compInv = comp . inv

  -- | 'invRev' 'p' returns the inverset inverse of the permutation 'p'.
  --
  -- prop> invRev p == compRev
  invRev :: Perm -> Perm
  invRev = comp . inv

  -- | 'invComp' 'p' returns the inverse complement of the permutation 'p'.
  --
  -- >>> invComp $ mk [1,3,4,2]
  -- [3,2,4,1]
  invComp :: Perm -> Perm
  invComp = inv . comp

  -- | 'revInv' 'p' returns the reverse inverse of the permutation 'p'.
  --
  -- prop> revInv p == invComp p
  revInv :: Perm -> Perm
  revInv = invComp

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
  extendLeft p = L.map fromListUnsafe [k : f k ys | k <- [1..len p+1]]
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
  prefix :: Int -> Perm -> Pattern
  prefix k = fromPointsUnsafe . L.take k . getPoints

  -- |'prefixes' 'p' returns all the prefixes of the permutation 'p' as
  -- permutations.
  --
  -- >>> p = mk [2,4,5,1,6,3]
  -- >>> prefixes p
  -- [[1],[1,2],[1,2,3],[2,3,4,1],[2,3,4,1,5],[2,4,5,1,6,3]]
  prefixes :: Perm -> [Pattern]
  prefixes = L.map fromPointsUnsafe . L.tail . L.inits . getPoints

  -- 'suffix' 'k' 'p' returns the suffix of length 'k' of the permutation 'p' as
  -- a permutation.
  --
  -- >>> p = mk [2,4,5,1,6,3]
  -- >>> [suffix i p | i <- [1..len p]]
  -- [[1],[2,1],[1,3,2],[3,1,4,2],[3,4,1,5,2],[2,4,5,1,6,3]]
  suffix :: Int -> Perm -> Pattern
  suffix k p = fromPointsUnsafe . L.drop (n-k) $ getPoints p
    where
      n = len p

  -- |'suffixes' 'p' returns all the suffixes of the permutation 'p' as
  -- permutations.
  --
  -- >>> p = mk [2,4,5,1,6,3]
  -- >>> suffixes p
  -- [[2,4,5,1,6,3],[3,4,1,5,2],[3,1,4,2],[1,3,2],[2,1],[1]]
  suffixes :: Perm -> [Pattern]
  suffixes = L.map fromPointsUnsafe . L.init . L.tails . getPoints

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
  patterns k = L.map fromPointsUnsafe . PP.Combi.subsets k . getPoints

  -- |'patterns'' 'k' 'p'
  patterns' :: Int -> Perm -> [Perm]
  patterns' k = L.map (fromList . toList) . patterns k

  -- |'maxPatterns' 'f' 'p' returns the longest patterns 'q' of permutation 'p'
  -- such that 'f' 'q' holds.
  maxpatterns' :: (Perm -> Bool) -> Perm -> [Perm]
  maxpatterns' f p = select $ L.dropWhile L.null [[q | q <- patterns' k p, f q] | k <- [n,n-1..1]]
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
  factors k = L.map fromPointsUnsafe . PP.Utils.List.chunk k . getPoints

  -- |'factors'' 'k' 'p' returns the list of all permutations of length 'k'
  -- that occur as a factor in the permutation 'p'.
  -- permutation 'p'.
  --
  -- >>> p = mk [2,4,1,3]
  -- >>> factors' 0 p
  -- []
  -- >>> factors' 1 p
  -- [[1],[1],[1],[1]]
  -- >>> factors' 2 p
  -- [[1,2],[1,2],[2,1]]
  -- >>> factors' 3 p
  -- [[2,3,1],[3,1,2]]
  -- >>> factors' 4 p
  -- [[2,4,1,3]]
  -- >>> factors' 5 p
  -- []
  factors' :: Int -> Perm -> [Perm]
  factors' k = L.map (mk . toList) . factors k

  maxfactors' :: (Perm -> Bool) -> Perm -> [Perm]
  maxfactors' f p = select $ L.dropWhile L.null [[q | q <- factors' k p, f q] | k <- [n,n-1..1]]
    where
      n         = len p
      select xs = if L.null xs then [] else L.head xs

  -- |'inversions' 'p' returns the inversions of the permutation 'p'.
  --
  -- >>> inversions (mk [1,5,3,2,6,4])
  -- [(5,3),(5,2),(5,4),(3,2),(6,4)]
  inversions :: Perm -> [(Y, Y)]
  inversions = L.map (\[i, j] -> (i, j)) . L.filter (\[i, j] -> i > j) . PP.Combi.subsets 2 . toList

  -- |'grid' 'p'
  --
  -- >>> putStr . grid  $ PP.Perm.mk [5,1,6,4,2,3]
  -- +---+---+---+---+---+---+
  -- |   |   | o |   |   |   |
  -- +---+---+---+---+---+---+
  -- | o |   |   |   |   |   |
  -- +---+---+---+---+---+---+
  -- |   |   |   | o |   |   |
  -- +---+---+---+---+---+---+
  -- |   |   |   |   |   | o |
  -- +---+---+---+---+---+---+
  -- |   |   |   |   | o |   |
  -- +---+---+---+---+---+---+
  -- |   | o |   |   |   |   |
  -- +---+---+---+---+---+---+
  grid :: Perm -> String
  grid p = aux . L.map (row . PP.Geometry.Point.getX) . L.sortBy (flip PP.Geometry.Point.cmpY) $ getPoints p
    where
      n     = len p
      sep   = ('+' :) $ F.concat (L.replicate n "---+") ++ "\n"
      row x = ('|' :) $ F.concat [F.concat (L.replicate (x-1) "   |"), " o |", F.concat (L.replicate (n-x) "   |"), "\n"]
      aux ss = sep ++ L.intercalate sep ss ++ sep
