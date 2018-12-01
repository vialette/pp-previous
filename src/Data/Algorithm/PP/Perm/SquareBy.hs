module Data.Algorithm.PP.Perm.SquareBy
(
  -- * Testing
  squareBy
, nonSquareBy
, kSquareByFree
, squareByFree

  -- * Generating
, squaresBy
, nonSquaresBy
, squaresByFree
)
where

  import qualified Control.Arrow as A
  import qualified Data.Foldable as F
  import qualified Data.List     as L

  import qualified Data.Algorithm.PP.Perm as PP.Perm

  -- |'squaresBy' 'f' 'n'
  squaresBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> Int -> [PP.Perm.Perm]
  squaresBy f = L.filter (squareBy f) . PP.Perm.perms

  -- |'nonSquaresBy' 'f' 'n'
  nonSquaresBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> Int -> [PP.Perm.Perm]
  nonSquaresBy f = L.filter (not . squareBy f) . PP.Perm.perms

  -- |'squaresByFree' 'f' n
  squaresByFree :: (PP.Perm.Perm -> PP.Perm.Perm) -> Int -> [PP.Perm.Perm]
  squaresByFree f = L.filter (squareByFree f) . PP.Perm.perms

  -- |'squareBy' 'f' 'p' returns 'True' if the permutation 'p' is the
  -- concatenation of two factors q and r such that q and f r are
  -- order-isomorphic.
  --
  -- >>> squareBy id (mk [3,4,1,5,6,2])
  -- True
  -- >>> (mk [3,4,1]) == (mk [5,6,2])
  -- True
  -- >>> squareBy inv (mk [3,4,1,6,2,5])
  -- True
  -- >>> (mk [3,4,1]) == inv (mk [6,2,5])
  -- True
  -- >>> squareBy rev (mk [3,4,1,2,6,5])
  -- True
  -- >>> (mk [3,4,1]) == rev (mk [2,6,5])
  -- True
  -- >>> squareBy comp (mk [3,4,1,5,2,6])
  -- True
  -- >>> (mk [3,4,1]) == comp (mk [5,2,6])
  -- True
  squareBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Bool
  squareBy f p = aux $ PP.Perm.getList p
    where
      n = PP.Perm.len p
      aux xs
        | odd n     = False
        | otherwise = uncurry (==) . (A.***) PP.Perm.mkPerm (f . PP.Perm.mkPerm) $ L.splitAt (n `div` 2) xs

  -- |'nonSquaresBy' 'f' 'p' returns 'True' if the permutation 'p' is not the
  -- concatenation of two factors 'q' and 'r' such that 'q' and 'f r' are
  -- order-isomorphic.
  nonSquareBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Bool
  nonSquareBy f = not . squareBy f

  -- |'kSquareByFree' 'f' 'k' 'p' return 'True' if the permutation 'p' does not contain
  -- a factor 'q' of length 'k' such that 'squareBy' 'f' 'q' is 'True'.
  kSquareByFree :: (PP.Perm.Perm -> PP.Perm.Perm) -> Int -> PP.Perm.Perm -> Bool
  kSquareByFree f k p
    | odd k     = True
    | otherwise = F.all (not . squareBy f . PP.Perm.mkPerm . PP.Perm.getList) $ PP.Perm.factors k p

  -- |'squareByFree' 'f' 'p' return 'True' if the permutation 'p' does not contain
  -- a factor 'q' of length at least 4 such that 'squareBy' 'f' 'q' is 'True'.
  squareByFree :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Bool
  squareByFree f p = F.and [kSquareByFree f k p | k <- [4,6..n]]
    where
      n = PP.Perm.len p
