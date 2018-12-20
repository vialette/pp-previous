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
  import qualified Data.Tuple    as T

  import qualified Data.Algorithm.PP.Perm           as PP.Perm
  import qualified Data.Algorithm.PP.Perm.Generator as PP.Perm.Generator
  import qualified Data.Algorithm.PP.Utils.List     as PP.Utils.List
  import qualified Data.Algorithm.PP.Utils.Foldable as PP.Utils.Foldable

  -- |'squareBy' 'f' 'p' returns @True@ if the permutation @p@ is @f@-square for some
  -- \(f : S_n \to S_n\).
  -- A permutation @p@ is @f@-square if @p = qr@, @|q| = |r|@ and @r = f q@$.
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

  -- |'nonSquaresBy' 'f' 'p' returns 'True' if the permutation 'p' is not
  -- 'f'-square.
  nonSquareBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Bool
  nonSquareBy f = not . squareBy f

  -- |'kSquareByFree' 'f' 'k' 'p' return 'True' if the permutation 'p' does not contain
  -- a 'f'-square factor 'q' of length 'k'
  -- (\(p = qrst\), \(|r| = |s| = k\) and \(f r = s\)).
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

  arrange f xs ys = L.map T.snd . L.sortOn T.fst $ L.zip ixs ys'
      where
        ixs = PP.Perm.getList . f . PP.Perm.mkPerm . L.map T.fst . L.sortOn T.snd $ L.zip [1..] xs
        ys' = L.sort ys

  -- |'squaresBy' 'f' 'n' returns all @f@-square permutations of length 'n' according
  -- to some one-to-one mapping \(f : S_n \to S_n\)
  -- (@p@ is a @f@-square permutation if @p = qr@ and @r = f q@).
  -- The function returns @[]@ is @n@ is odd.
  --
  -- >>> squaresBy id 4
  -- [[1,2,3,4],[1,3,2,4],[1,4,2,3],[2,1,4,3],[2,3,1,4],[2,4,1,3],[3,1,4,2],[3,2,4,1],[3,4,1,2],[4,1,3,2],[4,2,3,1],[4,3,2,1]]
  -- >>> squaresBy rev 4
  -- [[1,2,4,3],[1,3,4,2],[1,4,3,2],[2,1,3,4],[2,3,4,1],[2,4,3,1],[3,1,2,4],[3,2,1,4],[3,4,2,1],[4,1,2,3],[4,2,1,3],[4,3,1,2]]
  -- >>> squaresBy comp 4
  -- [[1,2,4,3],[1,3,4,2],[1,4,3,2],[2,1,3,4],[2,3,4,1],[2,4,3,1],[3,1,2,4],[3,2,1,4],[3,4,2,1],[4,1,2,3],[4,2,1,3],[4,3,1,2]]
  -- >>> squaresBy inv 4
  -- [[1,2,3,4],[1,3,2,4],[1,4,2,3],[2,1,4,3],[2,3,1,4],[2,4,1,3],[3,1,4,2],[3,2,4,1],[3,4,1,2],[4,1,3,2],[4,2,3,1],[4,3,2,1]]
  squaresBy :: Integral a =>  (PP.Perm.Perm -> PP.Perm.Perm) -> a -> [PP.Perm.Perm]
  squaresBy f n
    | odd n     = []
    | otherwise = PP.Utils.List.uniq . F.foldr aux [] . L.concatMap L.permutations . PP.Utils.Foldable.subsets (n `div` 2) $ [1..n]
      where
        aux xs acc = PP.Perm.mkPerm (xs ++ ys) : acc
          where
            ys = arrange f xs ([1..n] L.\\ xs)

  -- |'nonSquaresBy' 'f' 'n'
  nonSquaresBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> Int -> [PP.Perm.Perm]
  nonSquaresBy f = L.filter (not . squareBy f) . PP.Perm.Generator.perms

  -- |'squaresByFree' 'f' n
  squaresByFree :: (PP.Perm.Perm -> PP.Perm.Perm) -> Int -> [PP.Perm.Perm]
  squaresByFree f = L.filter (squareByFree f) . PP.Perm.Generator.perms
