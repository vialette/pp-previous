module Data.Algorithm.PP.Perm (
    -- * Type
    P
  , Perm
  , SubSeq

    -- * Building
  , mkPerm
  , mkPermUnsafe
  , fromPatt
  , fromPoints
  , fromList
  , mkSubSeq
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
  , kFactors
  , factors
  , maxFactors
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
  ) where

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
data Reduced

-- |'Sub' type
data Reducible

-- |'Perm' type
type Perm = P Reduced

-- |'Patt' type
type Patt = P Reducible

{- | 'mkPermUnsafe' @xs@.
Use with caution.

>>> mkPermUnsafe [1,2,4,3]
[1,2,4,3]
>>> mkPermUnsafe [1,2,1,2]
[1,2,1,2]
-}
mkPermUnsafe :: [Int] -> Perm
mkPermUnsafe = P . fmap (uncurry PP.Geometry.Point.mk) . L.zip [1..]

{- | 'mkPerm' @xs@ constructs a permutation from foldable @xs@.
Ties are resolved from left to right.

>>> mkPerm "abcd"
[1,2,3,4]
>>> mkPerm "abca"
[1,3,4,2]
>>> mkPerm "abab"
[1,3,2,4]
>>> mkPerm "aaaa"
[1,2,3,4]
-}
mkPerm :: (Foldable t, Ord a) => t a -> Perm
mkPerm = fromList . F.toList

{- | 'fromList' @xs@ returns the permutation that corresponds to @xs@ (ties are resolved from left to right).

>>> fromList "abcd"
[1,2,3,4]
>>> fromList "abca"
[1,3,4,2]
>>> fromList "abab"
[1,3,2,4]
>>> fromList "aaaa"
[1,2,3,4]
-}
fromList :: (Ord a) => [a] -> Perm
fromList = mkPermUnsafe . reduce

fromSubSeq :: Patt -> Perm
fromSubSeq = fromList . getList

-- |'fromPoints' 'ps' construct a permutation from a list of points.
-- The points do need to be sorted.
--
-- >>> fromPoints []
fromPoints :: (Foldable t) => t PP.Geometry.Point.Point -> Perm
fromPoints = mkPerm . fmap PP.Geometry.Point.getY . L.sortOn PP.Geometry.Point.getX . F.toList

-- | 'mkPatt' 'ps'
--
-- >>>
mkSubSeq :: [PP.Geometry.Point.Point] -> Patt
mkSubSeq = P

-- | 'getList' 'p' returns the list of the elements of permutation 'p'.
getList :: P a -> [Int]
getList = L.map PP.Geometry.Point.getY . getPoints

-- Reduce a list of elements.
reduce :: (Ord a) => [a] -> [Int]
reduce = L.map T.fst . L.sortBy cmpFstSnd . L.zip [1..] . L.sortBy cmpSnd . L.zip [1..]
  where
    cmpFstSnd = compare `on` (T.fst . T.snd)
    cmpSnd    = compare `on` T.snd



-- |'orderIso' 'p' 'q'
orderIso :: Perm-> P a -> Bool
orderIso p1 p2 = p1 == mkPerm (getList p2)

{- | 'len' @p@ returns the length of permutation @p@.
-}
len :: P a -> Int
len = L.length . getPoints

-- |'at'
at :: P a -> Int -> Int
at p = (L.!!) (getList p)

{- | 'prefix' @k p@ returns the prefix of length @k@ of permutation @p@.

>>> let p = mkPerm [2,4,5,1,6,3] in [prefix i p | i <- [1..len p]]
[[1],[1,2],[1,2,3],[2,3,4,1],[2,3,4,1,5],[2,4,5,1,6,3]]
-}
prefix :: Int -> P a -> SubSeq
prefix k = P . L.take k . getPoints

{- |'prefixes' 'p' returns all prefixes of permutation @p@.

>>> prefixes $ mkPerm [2,4,5,1,6,3]
[[1],[1,2],[1,2,3],[2,3,4,1],[2,3,4,1,5],[2,4,5,1,6,3]]
-}
prefixes :: P a -> [SubSeq]
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

{- | 'kFactors' @k@ @p@ returns the list of all factors of length @k@ of the
permutation @p@.

>>> kFactors 0 $ mkPerm [2,4,1,3]
[]
>>> kFactors 1 $ mkPerm [2,4,1,3]
[[2],[4],[1],[3]]
>>> kFactors 2 $ mkPerm [2,4,1,3]
[[2,4],[4,1],[1,3]]
>>> kFactors 3 $ mkPerm [2,4,1,3]
[[2,4,1],[4,1,3]]
>>> kFactors 4 $ mkPerm [2,4,1,3]
[[2,4,1,3]]
-}
kFactors :: Int -> P a -> [Patt]
kFactors k = L.map P . PP.Utils.List.chunk k . getPoints

{- | 'factors' @p@ retruns ll factors of the permutation @p@.

>>> factors $ mkPerm [2,4,1,3]
[[2],[4],[1],[3],[2,4],[4,1],[1,3],[2,4,1],[4,1,3],[2,4,1,3]]
-}
factors :: P a -> [Patt]
factors p = F.concat [kFactors k p |  k <- [1..len p]]

-- |'maxFactors' 'f' 'p'
maxFactors :: (Patt -> Bool) -> P a -> [Patt]
maxFactors f p = select $ L.dropWhile L.null [[q | q <- kFactors k p, f q] | k <- [n,n-1..1]]
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
