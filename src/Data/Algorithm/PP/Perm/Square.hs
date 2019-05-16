module Data.Algorithm.PP.Perm.Square (
    -- * Testing
    square
  , nonSquare
  , kSquareFree
  , squareFree

    -- * Generating
  , squares
  , nonSquares
  , squareFrees

    -- * Counting
  , kSquareCount
  , squareCount
  ) where

import qualified Control.Arrow as A
import qualified Data.Foldable as F
import qualified Data.List     as L

import qualified Data.Algorithm.PP.Perm          as PP.Perm
import qualified Data.Algorithm.PP.Perm.SquareBy as PP.Perm.SquareBy

{- | 'squares' @n@ returns all square permutations of length @n@.

>>> squares 2
[[1,2],[2,1]]
>>> squares 3
[]
>>> squares 4
[[1,2,3,4],[2,3,1,4],[1,3,2,4],[4,3,2,1],[3,2,4,1],[4,2,3,1],[1,4,2,3],[2,4,1,3],[2,1,4,3],[4,1,3,2],[3,4,1,2],[3,1,4,2]]
-}
squares :: Int -> [PP.Perm.Perm]
squares = PP.Perm.SquareBy.squaresBy id

{- | 'nonSquares' @n@ returns all non-square permutations of length @n@.

>>> nonSquares 2
[]
>>> nonSquares 3
[[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]
>>> nonSquares 4
[[2,1,3,4],[3,2,1,4],[3,1,2,4],[3,4,2,1],[2,4,3,1],[2,3,4,1],[4,1,2,3],[1,2,4,3],[4,2,1,3],[1,4,3,2],[1,3,4,2],[4,3,1,2]]
-}
nonSquares :: Int -> [PP.Perm.Perm]
nonSquares = PP.Perm.SquareBy.nonSquaresBy id

{- | 'squareFrees' @n@ returns all permutations of length @n@ that do not contain a square factor of length at least @4@.

>>> squareFrees 5
[[3,2,1,4,5],[3,4,2,1,5],[2,4,3,1,5],[4,2,1,3,5],[1,4,3,2,5],[4,3,1,2,5],[3,4,5,2,1],[5,2,3,4,1],[2,3,5,4,1],[3,2,4,5,1],[2,4,5,3,1],[4,2,3,5,1],[5,2,1,3,4],[2,1,3,5,4],[2,5,3,1,4],[1,5,3,2,4],[5,3,1,2,4],[3,1,2,5,4],[3,5,2,1,4],[1,4,5,3,2],[4,1,3,5,2],[4,5,3,1,2],[5,1,3,4,2],[1,3,5,4,2],[3,1,4,5,2],[3,5,4,1,2],[1,5,4,2,3],[5,4,1,2,3],[4,1,2,5,3],[4,5,2,1,3],[5,1,2,4,3],[1,2,5,4,3],[2,1,4,5,3],[2,5,4,1,3]]
>>> sort (squareFrees 5 ) == sort [p | p <- perms 5, squareFree p]
True
-}
squareFrees :: Int -> [PP.Perm.Perm]
squareFrees = PP.Perm.SquareBy.squaresByFree id

{- | 'square' @p@ returns true iff the permutation @p@ is the concatenation of two order-isomorphic factors.

>>> let p = mkPerm [1,2,3,4] in square p
True
>>> let p = mkPerm [2,1,3,4] in square p
False
-}
square :: PP.Perm.Perm -> Bool
square = PP.Perm.SquareBy.squareBy id

{- | 'nonSquare' @p@ returns true iff the permutation @p@ is not the concatenation of two order-isomorphic factors.

>>> nonSquare $ mkPerm [1,2,3,4]
False
>>> nonSquare $ mkPerm [2,1,3,4]
True
-}
nonSquare :: PP.Perm.Perm -> Bool
nonSquare = not . square

{- | 'kSquareFree' @k@ @p@ returns true iff the permutation @p@ does not contain a square factor of length @k@.
The function trivially returns @False@ if @k@ is odd.

>>> kSquareFree 2 $ mkPerm [3,5,6,2,1,4]
False
>>> kSquareFree 4 $ mkPerm [3,5,6,2,1,4]
True
>>> kSquareFree 6 $ mkPerm [3,5,6,2,1,4]
True
-}
kSquareFree :: Int -> PP.Perm.Perm -> Bool
kSquareFree = PP.Perm.SquareBy.kSquareByFree id

{- | 'squareFree' @p@ returns true iff the permutation @p@ does not contain a square factor of length at least 4.

>>> squareFree $ mkPerm [3,5,6,2,1,4]
True
>>> squareFree $ mkPerm [3,5,4,6,2,1]
False
-}
squareFree :: PP.Perm.Perm -> Bool
squareFree = PP.Perm.SquareBy.squareByFree id

{- | 'kSquareCount' @k@ @p@ returns the number of square factors of length @k@ in the permutation @p@.

prop> kSquareCount k p == length [q | q <- factors k p, square (mkPerm q)]

>>> kSquareCount 4 $ mkPerm [9,3,6,1,2,7,8,4,5]
4
>>> kSquareCount 6 $ mkPerm [9,3,6,1,2,7,8,4,5]
0
>>> kSquareCount 8 $ mkPerm [9,3,6,1,2,7,8,4,5]
1
-}
kSquareCount :: Int -> PP.Perm.Perm -> Int
kSquareCount = PP.Perm.SquareBy.kSquareByCount id

{- | 'squareCount' @p@ return the number of square factors of the permutation @p@.

prop> squareCount p == length [q | k <- [4..len p], q <- factors k p, square (fromPatt q)]

>>> squareCount $ mkPerm [9,3,6,1,2,7,8,4,5]
5
-}
squareCount :: PP.Perm.Perm -> Int
squareCount = PP.Perm.SquareBy.squareByCount id
