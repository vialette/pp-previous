{-|
Module      : Data.Algorithm.PP.Perm
Description : Permutations
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Permutations.
-}

module Data.Algorithm.PP.Perm (
    -- * Type
    Perm

    -- * Building permutations
  , mk
  , mkUnsafe
  , fromPoints
  , fromList
  , ascending
  , descending
  , identity
  , empty

  -- * Modifying
  , delete
  , deleteMin
  , deleteMax
  , deleteAt

    -- * Transforming
  , getPoints
  , getList

    -- * Querying
  , len
  , at

  , partitions
  , inversions

  -- * Querying
  , isIdentity

    -- * Displaying
  , grid
  , gridChar
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
mkUnsafe = Perm . fmap (uncurry PP.Geometry.Point.mk) . L.zip [1..]

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
getList :: Perm -> [Int]
getList = L.map PP.Geometry.Point.getY . getPoints

-- Reduce a list of elements.
reduce :: (Ord a) => [a] -> [Int]
reduce = L.map T.fst . L.sortBy cmpFstSnd . L.zip [1..] . L.sortBy cmpSnd . L.zip [1..]
  where
    cmpFstSnd = compare `on` (T.fst . T.snd)
    cmpSnd    = compare `on` T.snd

{- | 'len' @p@ returns the length of permutation @p@.
-}
len :: Perm -> Int
len = L.length . getPoints

{- | 'at' @i@ @p@ returns the integer at position @i@ in permutation @p@.

>>> (mk [1..4]) `at` 0
-}
at :: Int -> Perm -> Int
at i = flip (L.!!) i . getList

{- | 'delete' @i@ @p@ returns the permutations obtains by deleting @i@ in permutation @p@.
If @i@ is not part of permutation @p@, then the function returns @p@ unchanged.
-}
delete :: Int -> Perm -> Perm
delete i = mk . L.delete i . getList

{- | 'deleteMin' @p@ returns the permutations obtains by deleting @1@ in permutation @p@.
-}
deleteMin :: Perm -> Perm
deleteMin = delete 1

{- | 'deleteMax' @p@ returns the permutations obtains by deleting the maximum element in permutation @p@.
-}
deleteMax :: Perm -> Perm
deleteMax p = delete n p
  where
  n = len p

{- | 'deleteAt' @i@ @p@ returns the permutations obtains by deleting the element at position @i@
 in permutation @p@ (positions start at 0).
 if @i < 0@ or @i >= len p@ then the function returns Nothing.
-}
deleteAt :: Int -> Perm -> Maybe Perm
deleteAt i p
  | i <= 0 || i > len p = Nothing
  | otherwise           = Just . mk . L.delete i $ getList p

{- | 'partitions' @p@ @k@ @l@ returns all partitions @(qk,ql)@ of the permutation @p@ such that
@|qk|=k@ and @|ql|=l@.

>>>
-}
partitions :: Int -> Int -> Perm -> [([PP.Geometry.Point.Point], [PP.Geometry.Point.Point])]
partitions k l = PP.Utils.List.partitions k l . getPoints

{- | 'inversions' @p@ returns the inversions of the permutation @p@.

>>> inversions $ mk [1,5,3,2,6,4]
[(5,3),(5,2),(5,4),(3,2),(6,4)]
-}
inversions :: Perm -> [(Int, Int)]
inversions = L.filter (T.uncurry (>)) . PP.Utils.List.subsets2 . getList

{- | 'identity' @n@ returns the identity permutation of length @n@.

>>> identity 4
[1,2,3,4]
-}
identity :: Int -> Perm
identity = ascending

{- | 'ascending' @n@ returns the ascending permutation of length @n@.

>>> ascending 4
[1,2,3,4]
-}
ascending :: Int -> Perm
ascending n = mkUnsafe [1..n]

{- | 'descending' @n@ returns the ascending permutation of length @n@.

>>> descending 4
[4,3,2,1]
-}
descending :: Int -> Perm
descending n = mkUnsafe [n,n-1..1]

{- | 'empty' returns the empty permutation.
-}
empty :: Perm
empty = mkUnsafe []

{- | 'isIdentity' @p@ returns @True@ if the permutation @p@ is the identity.
-}
isIdentity :: Perm -> Bool
isIdentity p = True

{- | 'grid' @p@

>>> putStr . grid  $ mk [5,1,6,4,2,3]
+---+---+---+---+---+---+
|   |   | o |   |   |   |
+---+---+---+---+---+---+
| o |   |   |   |   |   |
+---+---+---+---+---+---+
|   |   |   | o |   |   |
+---+---+---+---+---+---+
|   |   |   |   |   | o |
+---+---+---+---+---+---+
|   |   |   |   | o |   |
+---+---+---+---+---+---+
|   | o |   |   |   |   |
+---+---+---+---+---+---+
>>> putStr . grid  $ identity 6
+---+---+---+---+---+---+
|   |   |   |   |   | o |
+---+---+---+---+---+---+
|   |   |   |   | o |   |
+---+---+---+---+---+---+
|   |   |   | o |   |   |
+---+---+---+---+---+---+
|   |   | o |   |   |   |
+---+---+---+---+---+---+
|   | o |   |   |   |   |
+---+---+---+---+---+---+
| o |   |   |   |   |   |
+---+---+---+---+---+---+
-}
grid :: Perm -> String
grid = gridChar 'o'

gridChar :: Char -> Perm -> String
gridChar c p = aux . L.map (row . PP.Geometry.Point.getX) . L.reverse . L.sortOn PP.Geometry.Point.getY $ getPoints p
  where
    n      = len p
    sep    = ('+' :) $ F.concat (L.replicate n "---+") ++ "\n"
    row x  = ('|' :) $ F.concat [F.concat (L.replicate (x-1) "   |"), " ", [c], " |", F.concat (L.replicate (n-x) "   |"), "\n"]
    aux ss = sep ++ L.intercalate sep ss ++ sep
