module Data.Algorithm.PP.Perm.SquareBy (
    -- * Testing
    squareBy
  , nonSquareBy
  , kSquareByFree
  , squareByFree

    -- * Generating
  , squaresBy
  , nonSquaresBy
  , squaresByFree

  -- * Counting
  , kSquareByCount
  , squareByCount
  ) where

import qualified Control.Arrow as A
import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T

import qualified Data.Algorithm.PP.Perm           as PP.Perm
import qualified Data.Algorithm.PP.Perm.Generator as PP.Perm.Generator
import qualified Data.Algorithm.PP.Utils.List     as PP.Utils.List
import qualified Data.Algorithm.PP.Utils.Foldable as PP.Utils.Foldable

{- | 'squareBy' @f@ @p@ returns @True@ if the permutation @p@ is a @f@-square for some
function \(f : S_n \to S_n\).

A permutation @p@ is @f@-square if @p = qr@, @|q| = |r|@ and @r = f q@.

>>> squareBy id $ mkPerm [3,4,1,5,6,2]
True
>>> mkPerm [3,4,1] == mkPerm [5,6,2]
True
>>> squareBy inv $ mkPerm [3,4,1,6,2,5]
True
>>> mkPerm [3,4,1] == inv (mkPerm [6,2,5])
True
>>> squareBy rev $ mkPerm [3,4,1,2,6,5]
True
>>> mkPerm [3,4,1] == rev (mkPerm [2,6,5])
True
>>> squareBy comp $ mkPerm [3,4,1,5,2,6]
True
>>> mkPerm [3,4,1] == comp (mkPerm [5,2,6])
True
-}
squareBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Bool
squareBy f p = aux $ PP.Perm.getList p
  where
    n = PP.Perm.len p
    aux xs
      | odd n     = False
      | otherwise = uncurry (==) . (A.***) PP.Perm.mkPerm (f . PP.Perm.mkPerm) $ L.splitAt (n `div` 2) xs

{- | 'nonSquaresBy' @f@ @p@ returns @True@ if the permutation @p@ is not
a @f@-square.

>>> nonSquareBy id $ mkPerm [3,4,1,6,5,2]
True
mkPerm [3,4,1] == mkPerm [6,5,2]
False
>>> nonSquareBy inv $ mkPerm [3,4,1,2,5,6]
True
>>> mkPerm [3,4,1] == inv (mkPerm [2,5,6])
False
>>> nonSquareBy rev $ mkPerm [3,4,1,2,5,6]
True
>>> mkPerm [3,4,1] == rev (mkPerm [2,5,6])
False
>>> nonSquareBy comp $ mkPerm [3,4,1,6,2,5]
True
>>> mkPerm [3,4,1] == comp (mkPerm [6,2,5])
False
-}
nonSquareBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Bool
nonSquareBy f = not . squareBy f

{- | 'kSquareByFree' @f@ @k@ @p@ return @True@ if the permutation @p@ does not contain
a @f@-square pattern @q@ of length @k@
(\(p = qrst\), \(|r| = |s| = k\) and \(f r = s\)).
-}
kSquareByFree :: (PP.Perm.Perm -> PP.Perm.Perm) -> Int -> PP.Perm.Perm -> Bool
kSquareByFree f k p
  | odd k     = True
  | otherwise = F.all (not . squareBy f . PP.Perm.mkPerm . PP.Perm.getList) $ PP.Perm.kFactors k p

{- |'squareByFree' @f@ @p@ return @True@ if the permutation @p@ does not contain
a pattern @q@ of length at least 4 such that 'squareBy' @f@ @q@ is @True@.
-}
squareByFree :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Bool
squareByFree f p = F.and [kSquareByFree f k p | k <- [4,6..n]]
  where
    n = PP.Perm.len p

-- 'squaresBy' helper function.
arrange f xs ys = L.map T.snd . L.sortOn T.fst $ L.zip ixs ys'
    where
      ixs = PP.Perm.getList . f . PP.Perm.mkPerm . L.map T.fst . L.sortOn T.snd $ L.zip [1..] xs
      ys' = L.sort ys

{- | 'squaresBy' @f@ @n@ returns all @f@-square permutations of length @n@ according
to the function \(f : S_n \to S_n\)
(@p@ is a @f@-square permutation if @p = qr@ and @r = f q@).
The function returns @[]@ is @n@ is odd.

>>> squaresBy id 4
[[1,2,3,4],[1,3,2,4],[1,4,2,3],[2,1,4,3],[2,3,1,4],[2,4,1,3],[3,1,4,2],[3,2,4,1],[3,4,1,2],[4,1,3,2],[4,2,3,1],[4,3,2,1]]
>>> squaresBy rev 4
[[1,2,4,3],[1,3,4,2],[1,4,3,2],[2,1,3,4],[2,3,4,1],[2,4,3,1],[3,1,2,4],[3,2,1,4],[3,4,2,1],[4,1,2,3],[4,2,1,3],[4,3,1,2]]
>>> squaresBy comp 4
[[1,2,4,3],[1,3,4,2],[1,4,3,2],[2,1,3,4],[2,3,4,1],[2,4,3,1],[3,1,2,4],[3,2,1,4],[3,4,2,1],[4,1,2,3],[4,2,1,3],[4,3,1,2]]
>>> squaresBy inv 4
[[1,2,3,4],[1,3,2,4],[1,4,2,3],[2,1,4,3],[2,3,1,4],[2,4,1,3],[3,1,4,2],[3,2,4,1],[3,4,1,2],[4,1,3,2],[4,2,3,1],[4,3,2,1]]
-}
squaresBy :: Integral a =>  (PP.Perm.Perm -> PP.Perm.Perm) -> a -> [PP.Perm.Perm]
squaresBy f n
  | odd n     = []
  | otherwise = PP.Utils.List.uniq . F.foldr aux [] . L.concatMap L.permutations . PP.Utils.Foldable.subsets (n `div` 2) $ [1..n]
    where
      aux xs acc = PP.Perm.mkPerm (xs ++ ys) : acc
        where
          ys = arrange f xs ([1..n] L.\\ xs)

{- | 'nonSquaresBy' @f@ @n@ returns all non @f@-square permutations of length @n@ according
to the function \(f : S_n \to S_n\)

>>> nonSquaresBy id 4
[[2,1,3,4],[3,2,1,4],[3,1,2,4],[3,4,2,1],[2,4,3,1],[2,3,4,1],[4,1,2,3],[1,2,4,3],[4,2,1,3],[1,4,3,2],[1,3,4,2],[4,3,1,2]]
>>> nonSquaresBy rev 4
[[1,2,3,4],[2,3,1,4],[1,3,2,4],[4,3,2,1],[3,2,4,1],[4,2,3,1],[1,4,2,3],[2,4,1,3],[2,1,4,3],[4,1,3,2],[3,4,1,2],[3,1,4,2]]
>>> nonSquaresBy comp 4
[[1,2,3,4],[2,3,1,4],[1,3,2,4],[4,3,2,1],[3,2,4,1],[4,2,3,1],[1,4,2,3],[2,4,1,3],[2,1,4,3],[4,1,3,2],[3,4,1,2],[3,1,4,2]]
>>> nonSquaresBy inv 4
[[2,1,3,4],[3,2,1,4],[3,1,2,4],[3,4,2,1],[2,4,3,1],[2,3,4,1],[4,1,2,3],[1,2,4,3],[4,2,1,3],[1,4,3,2],[1,3,4,2],[4,3,1,2]]
-}
nonSquaresBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> Int -> [PP.Perm.Perm]
nonSquaresBy f = L.filter (not . squareBy f) . PP.Perm.Generator.perms

{- |'squaresByFree' '@f@ @n@ returns all permutations of length @n@ that are
@f@-square-free.

>>> squaresByFree id 4
[[2,1,3,4],[3,2,1,4],[3,1,2,4],[3,4,2,1],[2,4,3,1],[2,3,4,1],[4,1,2,3],[1,2,4,3],[4,2,1,3],[1,4,3,2],[1,3,4,2],[4,3,1,2]]
>>> squaresByFree rev 4
[[1,2,3,4],[2,3,1,4],[1,3,2,4],[4,3,2,1],[3,2,4,1],[4,2,3,1],[1,4,2,3],[2,4,1,3],[2,1,4,3],[4,1,3,2],[3,4,1,2],[3,1,4,2]]
>>> squaresByFree comp 4
[[1,2,3,4],[2,3,1,4],[1,3,2,4],[4,3,2,1],[3,2,4,1],[4,2,3,1],[1,4,2,3],[2,4,1,3],[2,1,4,3],[4,1,3,2],[3,4,1,2],[3,1,4,2]]
>>> squaresByFree inv 4
[[2,1,3,4],[3,2,1,4],[3,1,2,4],[3,4,2,1],[2,4,3,1],[2,3,4,1],[4,1,2,3],[1,2,4,3],[4,2,1,3],[1,4,3,2],[1,3,4,2],[4,3,1,2]]
-}
squaresByFree :: (PP.Perm.Perm -> PP.Perm.Perm) -> Int -> [PP.Perm.Perm]
squaresByFree f = L.filter (squareByFree f) . PP.Perm.Generator.perms

{- | 'kSquareByCount' @f@ @k@ @p@ return the number of @f@-square factors of length @k@ of
the permutation @p@.

>>> kSquareByCount id 4 $ mkPerm [9,3,6,1,2,7,8,4,5]
4
>>> kSquareByCount id 6 $ mkPerm [9,3,6,1,2,7,8,4,5]
0
>>> kSquareByCount id 8 $ mkPerm [9,3,6,1,2,7,8,4,5]
1
>>> kSquareByCount rev 4 $ mkPerm [9,3,6,1,2,7,8,4,5]
2
>>> kSquareByCount rev 6 $ mkPerm [9,3,6,1,2,7,8,4,5]
0
>>> kSquareByCount rev 8 $ mkPerm [9,3,6,1,2,7,8,4,5]
0
>>> kSquareByCount comp 4 $ mkPerm [9,3,6,1,2,7,8,4,5]
2
>>> kSquareByCount comp 6 $ mkPerm [9,3,6,1,2,7,8,4,5]
0
>>> kSquareByCount comp 8 $ mkPerm [9,3,6,1,2,7,8,4,5]
0
>>> kSquareByCount inv 4 $ mkPerm [9,3,6,1,2,7,8,4,5]
4
>>> kSquareByCount inv 6 $ mkPerm [9,3,6,1,2,7,8,4,5]
1
>>> kSquareByCount inv 8 $ mkPerm [9,3,6,1,2,7,8,4,5]
1
-}
kSquareByCount :: (PP.Perm.Perm -> PP.Perm.Perm) -> Int -> PP.Perm.Perm -> Int
kSquareByCount f k p
  | odd k     = 0
  | otherwise = L.length [q | q <- PP.Perm.kFactors k p, squareBy f (PP.Perm.fromPatt q)]

{- | 'kSquareByCount' @f@ @k@ @p@ return the number of @f@-square factors of length @k@ of
the permutation @p@.

>>> squareByCount id $ mkPerm [9,3,6,1,2,7,8,4,5]
5
>>> squareByCount rev $ mkPerm [9,3,6,1,2,7,8,4,5]
2
>>> squareByCount comp $ mkPerm [9,3,6,1,2,7,8,4,5]
2
>>> squareByCount inv $ mkPerm [9,3,6,1,2,7,8,4,5]
6
-}
squareByCount :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Int
squareByCount f p = F.sum [kSquareByCount f k p | k <- [4,6..n]]
  where
    n = PP.Perm.len p
