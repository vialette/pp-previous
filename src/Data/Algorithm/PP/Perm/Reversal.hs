module Data.Algorithm.PP.Perm.Reversal
(
  reversal
, prefixReversal
, suffixReversal

, dReversal
, dReversalRadius
, dReversalRadiusFull
)
where

  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Tuple    as T

  import qualified Data.Algorithm.PP.Perm.Generator as PP.Perm.Generator
  import qualified Data.Algorithm.PP.Perm           as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List     as PP.Utils.List

  -- |'reversal' 'i' 'j' 'p'
  reversal :: Int -> Int -> PP.Perm.Perm -> PP.Perm.Perm
  reversal i j = PP.Perm.mkPerm . PP.Utils.List.reversal i j . PP.Perm.getList

  -- |'prefixReversal' 'm' 'p'
  prefixReversal :: Int -> PP.Perm.Perm -> PP.Perm.Perm
  prefixReversal = reversal 0

  -- |'suffixReversal' 'm' 'p'
  suffixReversal :: Int -> PP.Perm.Perm -> PP.Perm.Perm
  suffixReversal = reversal 0

  -- |'dReversals' 'p' returns the list of permutations 'q1', 'q2', ... 'qk'
  -- with 'p = q1'
  --
  -- >>> dReversal (mk [4,2,6,4,3,1])
  -- [4,2,6,5,3,1],[5,6,2,4,3,1],[3,4,2,6,5,1],[2,4,3,6,5,1],[4,2,3,6,5,1],[6,3,2,4,5,1],[1,5,4,2,3,6]]
  dReversal :: PP.Perm.Perm -> [PP.Perm.Perm]
  dReversal = L.map PP.Perm.mkPerm . L.reverse . aux [] . PP.Perm.getList
    where
      aux acc []         = acc
      aux acc (1 : xs)   = (1 : xs) : acc
      aux acc xs@(i : _) = aux (xs : acc) (PP.Utils.List.prefixReversal i xs)

  -- |'dReversalRadiusFull' 'n'
  --
  -- >>> dReversalRadiusFull 4
  -- (4,[[3,1,4,2],[2,4,1,3]])
  -- >>> mapM_ (putStr . (\ s -> s ++ "\n") . show) . map dReversal . T.snd $ dReversalRadiusFull 4
  -- [[3,1,4,2],[4,1,3,2],[2,3,1,4],[3,2,1,4],[1,2,3,4]]
  -- [[2,4,1,3],[4,2,1,3],[3,1,2,4],[2,1,3,4],[1,2,3,4]]
  -- >>> dReversalRadiusFull 5
  -- (7,[[3,1,4,5,2]])
  -- >>> mapM_ (putStr . (\ s -> s ++ "\n") . show) . map dReversal . T.snd $ dReversalRadiusFull 5
  -- [[3,1,4,5,2],[4,1,3,5,2],[5,3,1,4,2],[2,4,1,3,5],[4,2,1,3,5],[3,1,2,4,5],[2,1,3,4,5],[1,2,3,4,5]]
  -- >>> dReversalRadiusFull 6
  -- (10,[[4,5,6,2,1,3],[4,1,5,2,6,3],[4,1,6,5,2,3],[3,6,5,1,4,2],[5,6,4,1,3,2]])
  -- >>> mapM_ (putStr . (\ s -> s ++ "\n") . show) . map dReversal . T.snd $ dReversalRadiusFull 6
  -- [[4,5,6,2,1,3],[2,6,5,4,1,3],[6,2,5,4,1,3],[3,1,4,5,2,6],[4,1,3,5,2,6],[5,3,1,4,2,6],[2,4,1,3,5,6],[4,2,1,3,5,6],[3,1,2,4,5,6],[2,1,3,4,5,6],[1,2,3,4,5,6]]
  -- [[4,1,5,2,6,3],[2,5,1,4,6,3],[5,2,1,4,6,3],[6,4,1,2,5,3],[3,5,2,1,4,6],[2,5,3,1,4,6],[5,2,3,1,4,6],[4,1,3,2,5,6],[2,3,1,4,5,6],[3,2,1,4,5,6],[1,2,3,4,5,6]]
  -- [[4,1,6,5,2,3],[5,6,1,4,2,3],[2,4,1,6,5,3],[4,2,1,6,5,3],[6,1,2,4,5,3],[3,5,4,2,1,6],[4,5,3,2,1,6],[2,3,5,4,1,6],[3,2,5,4,1,6],[5,2,3,4,1,6],[1,4,3,2,5,6]]
  -- [[3,6,5,1,4,2],[5,6,3,1,4,2],[4,1,3,6,5,2],[6,3,1,4,5,2],[2,5,4,1,3,6],[5,2,4,1,3,6],[3,1,4,2,5,6],[4,1,3,2,5,6],[2,3,1,4,5,6],[3,2,1,4,5,6],[1,2,3,4,5,6]]
  -- [[5,6,4,1,3,2],[3,1,4,6,5,2],[4,1,3,6,5,2],[6,3,1,4,5,2],[2,5,4,1,3,6],[5,2,4,1,3,6],[3,1,4,2,5,6],[4,1,3,2,5,6],[2,3,1,4,5,6],[3,2,1,4,5,6],[1,2,3,4,5,6]]
  dReversalRadiusFull :: Int -> (Int, [PP.Perm.Perm])
  dReversalRadiusFull = F.foldr' f (0, []) . fmap ((\ ps -> (L.length ps - 1, L.head ps)) . dReversal) . PP.Perm.Generator.derangements
    where
      f (m, p) (maxSoFar, acc)
        | m > maxSoFar  = (m,         [p])
        | m == maxSoFar = (maxSoFar , p : acc)
        | otherwise     = (maxSoFar , acc)

  -- |'dReversalRadius' 'n' return the radius
  --
  -- >>> dReversalRadius 2
  -- 1
  -- >>> dReversalRadius 3
  -- 2
  -- >>> dReversalRadius 4
  -- 4
  -- >>> dReversalRadius 5
  -- 7
  -- >>> dReversalRadius 6
  -- 10
  dReversalRadius :: Int -> Int
  dReversalRadius = T.fst . dReversalRadiusFull
