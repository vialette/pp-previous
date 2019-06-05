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
  , fromPoints
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

-- | 'Perm' type
newtype Perm = Perm { getPoints :: [PP.Geometry.Point.Point] }
               deriving (Eq, Ord)

-- |
instance Show Perm where
  show = show . fmap PP.Geometry.Point.getY . getPoints

{- | 'mk' @xs@ constructs a permutation from a list of distinct integers.
The function does not check the argument. Use with caution.

-}
mk :: (Ord a) => [a] -> Perm
mk = Perm .L.zipWith PP.Geometry.Point.mk [1 ..] . reduce

{- |'fromPoints' @ps@ construct a permutation from a list of points. The points do need to be sorted.

>>> fromPoints []
-}
fromPoints :: (Foldable t) => t PP.Geometry.Point.Point -> Perm
fromPoints = mk . fmap PP.Geometry.Point.getY . L.sortOn PP.Geometry.Point.getX . F.toList

{- | 'getList' @p@ returns the list of the elements of permutation @p@.

>>> getList $ mk [1,5,3]
[1,3,2]
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
The first element is at position 1.

>>> let p = mk [1..4] in [p `at` i | i <- [1 .. len p]]
[1,2,3,4]
>>> mk [1..4] `at` 0
*** Exception: Prelude.!!: negative index
>>> mk [1 .. 4] `at` 5
*** Exception: Prelude.!!: index too large
-}
at :: Perm -> Int -> Int
at p i = PP.Geometry.Point.getY . flip (L.!!) (i-1) $ getPoints p

{- | 'delete' @i@ @p@ returns the permutations obtains by deleting @i@ in permutation @p@.
If @i@ is not part of permutation @p@, then the function returns @p@ unchanged.

>>> let p = mk [4,1,3,2] in [delete i p | i <- [1 .. len p]]
[[3,2,1],[3,1,2],[3,1,2],[1,3,2]]
-}
delete :: Int -> Perm -> Maybe Perm
delete i p
  | i < 1 || i > len p = Nothing
  | otherwise          = Just . mk . L.delete i $ getList p

{- | 'deleteMin' @p@ returns the permutations obtains by deleting @1@ in permutation @p@.

>>> deleteMin $ mk [4,1,3,2]
[3,2,1]
>>> let p = mk [4,1,3,2] in deleteMin p == delete 1 p
True
-}
deleteMin :: Perm -> Maybe Perm
deleteMin = delete 1

{- | 'deleteMax' @p@ returns the permutations obtains by deleting the maximum element in permutation @p@.

>>> deleteMax $ mk [4,1,3,2]
[1,3,2]
>>> let p = mk [4,1,3,2] in deleteMax p == delete 4 p
True
-}
deleteMax :: Perm -> Maybe Perm
deleteMax p = delete (len p) p

{- | 'deleteAt' @i@ @p@ returns the permutations obtains by deleting the element at position @i@
in permutation @p@ (positions start at 0).
if @i < 0@ or @i >= len p@ then the function returns Nothing.

>>> let p = mk [4,1,3,2] in [deleteAt i p | i <- [0..len p+1]]
[Nothing,Just [3,2,1],Just [3,1,2],Just [3,1,2],Just [1,3,2],Nothing]
-}
deleteAt :: Int -> Perm -> Maybe Perm
deleteAt i p
  | i <= 0 || i > len p = Nothing
  | otherwise           = Just . mk . L.delete i $ getList p

{- | 'partitions' @p@ @k@ @l@ returns all partitions @(qk,ql)@ of the permutation @p@ such that
@|qk|=k@ and @|ql|=l@.

>>> let n = 4; p = mk [1..n] in mapM_ print [(k, l, partitions k l p) | k <- [0..n], l <- [0..n], k+l == n]
(0,4,[([],[(1,1),(2,2),(3,3),(4,4)])])
(1,3,[([(1,1)],[(2,2),(3,3),(4,4)]),([(2,2)],[(1,1),(3,3),(4,4)]),([(3,3)],[(1,1),(2,2),(4,4)]),([(4,4)],[(1,1),(2,2),(3,3)])])
(2,2,[([(1,1),(2,2)],[(3,3),(4,4)]),([(1,1),(3,3)],[(2,2),(4,4)]),([(1,1),(4,4)],[(2,2),(3,3)]),([(2,2),(3,3)],[(1,1),(4,4)]),([(2,2),(4,4)],[(1,1),(3,3)]),([(3,3),(4,4)],[(1,1),(2,2)])])
(3,1,[([(1,1),(2,2),(3,3)],[(4,4)]),([(1,1),(2,2),(4,4)],[(3,3)]),([(1,1),(3,3),(4,4)],[(2,2)]),([(2,2),(3,3),(4,4)],[(1,1)])])
(4,0,[([(1,1),(2,2),(3,3),(4,4)],[])])
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
ascending n = mk [1..n]

{- | 'descending' @n@ returns the ascending permutation of length @n@.

>>> descending 4
[4,3,2,1]
-}
descending :: Int -> Perm
descending n = mk [n,n-1..1]

{- | 'empty' returns the empty permutation.
-}
empty :: Perm
empty = Perm []

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
>>> putStr . grid $ identity 6
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

{- | 'gridChar' @c@ @p@

>>> putStr . gridChar 'x' $ mk [5,1,6,4,2,3]
    +---+---+---+---+---+---+
    |   |   | x |   |   |   |
    +---+---+---+---+---+---+
    | x |   |   |   |   |   |
    +---+---+---+---+---+---+
    |   |   |   | x |   |   |
    +---+---+---+---+---+---+
    |   |   |   |   |   | x |
    +---+---+---+---+---+---+
    |   |   |   |   | x |   |
    +---+---+---+---+---+---+
    |   | x |   |   |   |   |
    +---+---+---+---+---+---+
>>> putStr . gridChar '1' $ identity 6
    +---+---+---+---+---+---+
    |   |   |   |   |   | 1 |
    +---+---+---+---+---+---+
    |   |   |   |   | 1 |   |
    +---+---+---+---+---+---+
    |   |   |   | 1 |   |   |
    +---+---+---+---+---+---+
    |   |   | 1 |   |   |   |
    +---+---+---+---+---+---+
    |   | 1 |   |   |   |   |
    +---+---+---+---+---+---+
    | 1 |   |   |   |   |   |
    +---+---+---+---+---+---+
-}
gridChar :: Char -> Perm -> String
gridChar c p = aux . L.map (row . PP.Geometry.Point.getX) . L.reverse . L.sortOn PP.Geometry.Point.getY $ getPoints p
  where
    n      = len p
    sep    = ('+' :) $ F.concat (L.replicate n "---+") ++ "\n"
    row x  = ('|' :) $ F.concat [F.concat (L.replicate (x-1) "   |"), " ", [c], " |", F.concat (L.replicate (n-x) "   |"), "\n"]
    aux ss = sep ++ L.intercalate sep ss ++ sep
