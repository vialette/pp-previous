module Data.Algorithm.PP.Perm.Square
(
  -- * Testing
  square
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

  import qualified Data.Algorithm.PP.Perm as PP.Perm
  import qualified Data.Algorithm.PP.Perm.SquareBy as PP.Perm.SquareBy

  squares :: Int -> [PP.Perm.Perm]
  squares = PP.Perm.SquareBy.squaresBy id

  nonSquares :: Int -> [PP.Perm.Perm]
  nonSquares = PP.Perm.SquareBy.nonSquaresBy id

  -- |'square' p' returns 'True' if the permutation 'p' is the concatenation
  -- of two factors 'q' and 'r' such that 'q' and 'r' are order-isomorphic.
  square :: PP.Perm.Perm -> Bool
  square = PP.Perm.SquareBy.squareBy id

  -- |'kSquareFree' 'k' 'p' return 'True' if the permutation 'p' does not contain
  -- a square factor of length 'k'.
  kSquareFree :: Int -> PP.Perm.Perm -> Bool
  kSquareFree = PP.Perm.SquareBy.kSquareFreeBy id

  -- |'squareFree' 'p' returns 'True' if the permutation 'p' does not contain
  -- a square factor of length at least 4.
  squareFree :: PP.Perm.Perm -> Bool
  squareFree = PP.Perm.SquareBy.squareFreeBy id
