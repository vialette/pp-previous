module Data.Algorithm.PP.Perm.Square
(
  squareFun
, kSquareFreeFun
, squareFreeFun

, square
, kSquareFree
, squareFree
)
where

  import qualified Control.Arrow as A
  import qualified Data.Foldable as F
  import qualified Data.List     as L

  import qualified Data.Algorithm.PP.Perm as PP.Perm

  -- |'square' p' returns 'True' if the permutation 'p' is the concatenation
  -- of two factors 'q' and 'r' such that 'q' and 'r' are order-isomorphic.
  square :: PP.Perm.Perm -> Bool
  square = squareFun id

  -- |'kSquareFree' 'k' 'p' return 'True' if the permutation 'p' does not contain
  -- a square factor of length 'k'.
  kSquareFree :: Int -> PP.Perm.Perm -> Bool
  kSquareFree = kSquareFreeBy id

  -- |'squareFree' 'p' returns 'True' if the permutation 'p' does not contain
  -- a square factor of length at least 4.
  squareFree :: PP.Perm.Perm -> Bool
  squareFree = squareFreeBy id
