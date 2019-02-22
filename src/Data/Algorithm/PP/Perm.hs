module Data.Algorithm.PP.Perm
(
  -- * Type
  P
, Perm
, Patt
, FPerm

  -- * Building
, mkPerm
, mkPermUnsafe
, fromPoints
, fromList
, mkPatt
, identity
, empty

  -- * Transforming
, getPoints
, getList

  -- * Comparing
, orderIso

  -- * Querying
, len
, at
, prefix
, prefixes
, suffix
, suffixes
, factors
, factors'
, maxfactors
, patterns
, patterns'
, maxPatterns

, partitions
, inversions

-- * Querying
, isIdentity


  -- * Displaying
, grid
, grid'
)
where

  import qualified Control.Arrow as A
  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Tuple    as T
  import Data.Function (on)

  import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point
  import qualified Data.Algorithm.PP.Utils.Foldable as PP.Utils.Foldable
  import qualified Data.Algorithm.PP.Utils.List     as PP.Utils.List

  -- |'P' type
  newtype P a = P { getPoints :: [PP.Geometry.Point.Point] }

  -- |
  instance Show (P a) where
    show = show . fmap PP.Geometry.Point.getY . getPoints

  -- |
  instance Eq (P a) where
    p == q = f p == f q
      where
        f = getPoints . fromList . getList

  -- |
  instance Ord (P a) where
    p `compare` q = f p `compare` f q
      where
        f = getPoints . fromList . getList

  -- |'Span' type
  data Span

  -- |'Sub' type
  data Sub

  -- |'Perm' type
  type Perm = P Span

  -- |'Patt' type
  type Patt = P Sub

  -- |'Perm' to 'Perm' function type.
  type FPerm = Perm -> Perm

  -- |'mkPermUnsafe' 'xs'
  mkPermUnsafe :: [Int] -> Perm
  mkPermUnsafe = P . fmap (uncurry PP.Geometry.Point.mk) . L.zip [1..]

  {- | 'mkPerm' @xs@ constructs a permutation from foldable @xs@.
  Ties are resolved from left to right.

  >>> mkPerm ['a','c','e','d','b']
  [1,3,5,4,2]
  >>> mkPerm (take 5 ['a'..])
  [1,2,3,4,5]
  >>> mkPerm . take 5 $ repeat 'a'
  [1,2,3,4,5]
  -}
  mkPerm :: (Foldable t, Ord a) => t a -> Perm
  mkPerm = fromList . F.toList

  fromList :: (Ord a) => [a] -> Perm
  fromList = mkPermUnsafe . reduce

  -- |'fromPoints' 'ps' construct a permutation from a list of points.
  -- The points do need to be sorted.
  --
  -- >>> fromPoints []
  fromPoints :: (Foldable t) => t PP.Geometry.Point.Point -> Perm
  fromPoints = mkPerm . fmap PP.Geometry.Point.getY . L.sortOn PP.Geometry.Point.getX . F.toList

  -- Reduce a list of elements.
  reduce :: (Ord a) => [a] -> [Int]
  reduce = L.map T.fst . L.sortBy cmpFstSnd . L.zip [1..] . L.sortBy cmpSnd . L.zip [1..]
    where
      cmpFstSnd = compare `on` (T.fst . T.snd)
      cmpSnd    = compare `on` T.snd

  -- | 'mkPatt' 'ps'
  --
  -- >>>
  mkPatt :: [PP.Geometry.Point.Point] -> Patt
  mkPatt = P

  -- | 'getList' 'p' returns the list of the elements of permutation 'p'.
  getList :: P a -> [Int]
  getList = L.map PP.Geometry.Point.getY . getPoints

  -- |'orderIso' 'p' 'q'
  orderIso :: Perm-> P a -> Bool
  orderIso p1 p2 = p1 == mkPerm (getList p2)

  {- | 'len' @p@ returns the length of @p@.
  -}
  len :: P a -> Int
  len = L.length . getPoints

  -- |'at'
  at :: P a -> Int -> Int
  at p = (L.!!) (getList p)

  {- | 'prefix' @k p@ returns the prefix of length @k@ of @p@.

  >>> let p = mk [2,4,5,1,6,3] in [prefix i p | i <- [1..len p]]
  [[1],[1,2],[1,2,3],[2,3,4,1],[2,3,4,1,5],[2,4,5,1,6,3]]
  -}
  prefix :: Int -> P a -> Patt
  prefix k = P . L.take k . getPoints

  -- |'prefixes' 'p' returns all the prefixes
  --
  -- >>> p = mk [2,4,5,1,6,3]
  -- >>> prefixes p
  -- [[1],[1,2],[1,2,3],[2,3,4,1],[2,3,4,1,5],[2,4,5,1,6,3]]
  prefixes :: P a -> [Patt]
  prefixes = L.map P . L.tail . L.inits . getPoints

  -- 'suffix' 'k' 'p' returns the suffix of length 'k' of the permutation 'p' as
  -- a permutation.
  --
  -- >>> p = mk [2,4,5,1,6,3]
  -- >>> [suffix i p | i <- [1..len p]]
  -- [[1],[2,1],[1,3,2],[3,1,4,2],[3,4,1,5,2],[2,4,5,1,6,3]]
  suffix :: Int -> P a -> Patt
  suffix k p = P . L.drop (n-k) $ getPoints p
    where
      n = len p

  -- |'suffixes' 'p' returns all the suffixes of the permutation 'p' as
  -- permutations.
  --
  -- >>> p = mk [2,4,5,1,6,3]
  -- >>> suffixes p
  -- [[2,4,5,1,6,3],[3,4,1,5,2],[3,1,4,2],[1,3,2],[2,1],[1]]
  suffixes :: P a -> [Patt]
  suffixes = L.map P . L.init . L.tails . getPoints

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
  factors :: Int -> P a -> [Patt]
  factors k = L.map P . PP.Utils.List.chunk k . getPoints

  -- |'factors'' 'p'
  factors' :: P a -> [Patt]
  factors' p = F.concat [factors k p |  k <- [1..len p]]

  -- |'maxfactors' 'f' 'p'
  maxfactors :: (Patt -> Bool) -> P a -> [Patt]
  maxfactors f p = select $ L.dropWhile L.null [[q | q <- factors k p, f q] | k <- [n,n-1..1]]
    where
      n         = len p
      select xs = if L.null xs then [] else L.head xs

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
  patterns :: Int -> P a -> [Perm]
  patterns k = L.map P . PP.Utils.Foldable.subsets k . getPoints

  -- |'patterns'' 'k' 'p'
  patterns' :: Int -> P a -> [Perm]
  patterns' k = L.map (P . getPoints) . patterns k

  -- |'maxPatterns' 'f' 'p' returns the longest patterns 'q' of permutation 'p'
  -- such that 'f' 'q' holds.
  maxPatterns :: (Perm-> Bool) -> P a -> [Perm]
  maxPatterns f p = select $ L.dropWhile L.null [[q | q <- patterns k p, f q] | k <- [n,n-1..1]]
    where
      n         = len p
      select ys = if L.null ys then [] else L.head ys

  -- |'partitions' 'p' 'k' 'l' returns all partitions ('qk','ql') of the permutation
  -- 'p' such that '|qk|=k' and '|ql|=l'
  --
  -- >>>
  partitions :: Int -> Int -> P a -> [(Patt, Patt)]
  partitions k l = L.map ((A.***) P P) . PP.Utils.Foldable.partitions k l . getPoints

  -- |'inversions' 'p' returns the inversions of the permutation 'p'.
  --
  -- >>> inversions (mk [1,5,3,2,6,4])
  -- [(5,3),(5,2),(5,4),(3,2),(6,4)]
  inversions :: P a -> [(Int, Int)]
  inversions = L.map (\[i, j] -> (i, j)) . L.filter (\[i, j] -> i > j) . PP.Utils.Foldable.subsets 2 . getList

  -- | 'identity' 'n' returns the identity permutation of length 'n'.
  --
  -- >>> identity 4
  -- [1,2,3,4]
  identity :: Int -> Perm
  identity n = mkPermUnsafe [1..n]

  empty :: Perm
  empty = mkPermUnsafe []


  isIdentity :: P a -> Bool
  isIdentity p = True

  -- |'grid' 'p'
  --
  -- >>> putStr $ grid  (mkPerm [5,1,6,4,2,3])
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
  -- >>> putStr $ grid  (identity 6)
  -- +---+---+---+---+---+---+
  -- |   |   |   |   |   | o |
  -- +---+---+---+---+---+---+
  -- |   |   |   |   | o |   |
  -- +---+---+---+---+---+---+
  -- |   |   |   | o |   |   |
  -- +---+---+---+---+---+---+
  -- |   |   | o |   |   |   |
  -- +---+---+---+---+---+---+
  -- |   | o |   |   |   |   |
  -- +---+---+---+---+---+---+
  -- | o |   |   |   |   |   |
  -- +---+---+---+---+---+---+
  grid :: Perm -> String
  grid p = aux . L.map (row . PP.Geometry.Point.getX) . L.reverse . L.sortOn PP.Geometry.Point.getY $ getPoints p
    where
      n      = len p
      sep    = ('+' :) $ F.concat (L.replicate n "---+") ++ "\n"
      row x  = ('|' :) $ F.concat [F.concat (L.replicate (x-1) "   |"), " o |", F.concat (L.replicate (n-x) "   |"), "\n"]
      aux ss = sep ++ L.intercalate sep ss ++ sep

  grid' :: Perm -> String
  grid' p = ""
