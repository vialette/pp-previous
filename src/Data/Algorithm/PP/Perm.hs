module Data.Algorithm.PP.Perm (
    -- * Type
  , Perm

    -- * Building permutations
  , mk
  , mkUnsafe
  , fromPoints
  , fromList
  , identity
  , empty

    -- * Transforming
  , getPoints
  , getList

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

-- |'Perm' type
newtype Perm = Perm { getPoints :: [PP.Geometry.Point.Point] }

-- |
instance Show Perm where
  show = show . fmap PP.Geometry.Point.getY . getPoints

-- |
instance Eq Perm where
  p == q = f p == f q
    where
      f = getPoints . fromList . getList

-- |
instance Ord Perm where
  p `compare` q = f p `compare` f q
    where
      f = getPoints . fromList . getList

{- | 'mkUnsafe' @xs@.
Use with caution.

>>> mkUnsafe [1,2,4,3]
[1,2,4,3]
>>> mkUnsafe [1,2,1,2]
[1,2,1,2]
-}
mkUnsafe :: [Int] -> Perm
mkUnsafe = P . fmap (uncurry PP.Geometry.Point.mk) . L.zip [1..]

{- | 'mk' @xs@ constructs a permutation from foldable @xs@ (ties are resolved from left to right).

>>> mk "abcd"
[1,2,3,4]
>>> mk "abca"
[1,3,4,2]
>>> mk "abab"
[1,3,2,4]
>>> mk "aaaa"
[1,2,3,4]
-}
mk :: (Foldable t, Ord a) => t a -> Perm
mk = fromList . F.toList

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
fromList = mkUnsafe . reduce

{- |'fromPoints' @ps@ construct a permutation from a list of points. The points do need to be sorted.

>>> fromPoints []
-}
fromPoints :: (Foldable t) => t PP.Geometry.Point.Point -> Perm
fromPoints = mk . fmap PP.Geometry.Point.getY . L.sortOn PP.Geometry.Point.getX . F.toList

{- | 'getList' @p@ returns the list of the elements of permutation @p@.

>>> getList $ mkPerm [1,5,3]

-}
getList :: P a -> [Int]
getList = L.map PP.Geometry.Point.getY . getPoints

-- Reduce a list of elements.
reduce :: (Ord a) => [a] -> [Int]
reduce = L.map T.fst . L.sortBy cmpFstSnd . L.zip [1..] . L.sortBy cmpSnd . L.zip [1..]
  where
    cmpFstSnd = compare `on` (T.fst . T.snd)
    cmpSnd    = compare `on` T.snd

{- | 'len' @p@ returns the length of permutation @p@.
-}
len :: P a -> Int
len = L.length . getPoints

{- | 'at' @i@ @p@ returns the integer at position @i@ in permutation @p@.
-}
at :: P a -> Int -> Int
at p = (L.!!) (getList p)


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
identity n = mkUnsafe [1..n]

empty :: Perm
empty = mkUnsafe []


isIdentity :: P a -> Bool
isIdentity p = True

-- |'grid' 'p'
--
-- >>> putStr $ grid  (mk [5,1,6,4,2,3])
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
