module Data.Algorithm.PP.Perm.Square
(
  -- * Testing
  square
, nonSquare
, kSquareFree
, squareFree

  -- * Generating
, squares
, nonSquares
)
where

  import qualified Control.Arrow as A
  import qualified Data.Foldable as F
  import qualified Data.List     as L

  import qualified Data.Algorithm.PP.Perm          as PP.Perm
  import qualified Data.Algorithm.PP.Perm.SquareBy as PP.Perm.SquareBy

  -- |'squares' 'n' return all square permutations of length 'n'.
  --
  -- >>> squares 4
  -- [[1,2,3,4],[2,3,1,4],[1,3,2,4],[4,3,2,1],[3,2,4,1],[4,2,3,1],[1,4,2,3],[2,4,1,3],[2,1,4,3],[4,1,3,2],[3,4,1,2],[3,1,4,2]]
  squares :: Int -> [PP.Perm.Perm]
  squares = PP.Perm.SquareBy.squaresBy id

  -- |'nonSquares' 'n' return all non-square permutations of length 'n'.
  --
  -- >>> nonSquares 4
  -- [[2,1,3,4],[3,2,1,4],[3,1,2,4],[3,4,2,1],[2,4,3,1],[2,3,4,1],[4,1,2,3],[1,2,4,3],[4,2,1,3],[1,4,3,2],[1,3,4,2],[4,3,1,2]]
  nonSquares :: Int -> [PP.Perm.Perm]
  nonSquares = PP.Perm.SquareBy.nonSquaresBy id

  squareFrees:: Int -> [PP.Perm.Perm]
  squareFrees = PP.Perm.SquareBy.squaresByFree id

  -- |'square' 'p' returns 'True' if the permutation 'p' is the concatenation
  -- of two order-isomorphic factors 'q' and 'r'.
  --
  -- >>> square (mk [1,2,3,4])
  -- True
  -- >>> square (mk [2,1,3,4])
  -- False
  square :: PP.Perm.Perm -> Bool
  square = PP.Perm.SquareBy.squareBy id

  -- |'nonSquare' 'p' returns 'True' if the permutation 'p' is not the concatenation
  -- of two order-isomorphic factors 'q' and 'r'.
  --
  -- >>> square (mk [1,2,3,4])
  -- False
  -- >>> square (mk [2,1,3,4])
  -- True
  nonSquare :: PP.Perm.Perm -> Bool
  nonSquare = not . square

  -- |'kSquareFree' 'k' 'p' return 'True' if the permutation 'p' does not contain
  -- a square factor of length 'k'. The function trivially returns 'False' in case
  -- 'k' is odd.
  --
  -- >>> kSquareFree 2 (mk [3,5,6,2,1,4])
  -- False
  -- >>> kSquareFree 4 (mk [3,5,6,2,1,4])
  -- True
  -- >>> kSquareFree 6 (mk [3,5,6,2,1,4])
  -- True
  kSquareFree :: Int -> PP.Perm.Perm -> Bool
  kSquareFree = PP.Perm.SquareBy.kSquareByFree id

  -- |'squareFree' 'p' returns 'True' if the permutation 'p' does not contain
  -- a square factor of length at least 4.
  --
  -- >>> squareFree (mk [3,5,6,2,1,4])
  -- True
  -- >>> squareFree (mk [3,5,4,6,2,1])
  -- False
  squareFree :: PP.Perm.Perm -> Bool
  squareFree = PP.Perm.SquareBy.squareByFree id
