module Data.Algorithm.PP.Perm.SquareBy
(
  -- * Testing
  squareBy
, kSquareFreeBy
, squareFreeBy

  -- * Generating
, squaresBy
, nonSquaresBy
)
where

  import qualified Control.Arrow as A
  import qualified Data.Foldable as F
  import qualified Data.List     as L

  import qualified Data.Algorithm.PP.Perm as PP.Perm

  squaresBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> Int -> [PP.Perm.Perm]
  squaresBy f = L.filter (squareBy f) . PP.Perm.perms

  nonSquaresBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> Int -> [PP.Perm.Perm]
  nonSquaresBy f = L.filter (not . squareBy f) . PP.Perm.perms


  -- |'squareBy' 'f' 'p' returns 'True' if the permutation 'p' is the concatenation
  -- of two factors 'q' and 'r' such that 'q' and 'f' 'r' are order-isomorphic.
  squareBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Bool
  squareBy f p = aux $ PP.Perm.toList p
    where
      n = PP.Perm.len p
      aux xs
        | odd n     = False
        | otherwise = uncurry (==) . (A.***) PP.Perm.mk (f . PP.Perm.mk) $ L.splitAt (n `div` 2) xs

  -- |'kSquareFreeBy' 'f' 'k' 'p' return 'True' if the permutation 'p' does not contain
  -- a factor 'q' of length 'k' such that 'squareBy' 'f' 'q' is 'True'.
  kSquareFreeBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> Int -> PP.Perm.Perm -> Bool
  kSquareFreeBy f k p
    | odd k     = True
    | otherwise = F.all (not . squareBy f) $ PP.Perm.permFactors k p

  -- |'squareFreeBy' 'f' 'p' return 'True' if the permutation 'p' does not contain
  -- a factor 'q' of length at least 4 such that 'squareBy' 'f' 'q' is 'True'.
  squareFreeBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Bool
  squareFreeBy f p = F.and [kSquareFreeBy f k p | k <- [4,6..n]]
    where
      n = PP.Perm.len p
