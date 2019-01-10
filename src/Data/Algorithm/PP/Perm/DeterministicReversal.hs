module Data.Algorithm.PP.Perm.DeterministicReversal
(
  deterministicReversals
, deterministicReversalRadius
, longestDeterministicReversals
, label
)
where

  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Tuple    as T

  import qualified Data.Algorithm.PP.Perm            as PP.Perm
  import qualified Data.Algorithm.PP.Geometry.Point  as PP.Geometry.Point
  import qualified Data.Algorithm.PP.Perm.Generator  as PP.Perm.Generator
  import qualified Data.Algorithm.PP.Perm.Statistics as PP.Perm.Statistics
  import qualified Data.Algorithm.PP.Utils.Integer   as PP.Utils.Integer
  import qualified Data.Algorithm.PP.Utils.List      as PP.Utils.List


  label :: PP.Perm.Perm -> Integer
  label = PP.Utils.Integer.encode . fmap PP.Geometry.Point.getY . PP.Perm.Statistics.fixedPoints

  -- |'deterministicReversals' 'p' returns the list of permutations 'q1', 'q2', ... 'qk'
  -- with 'p = q1'
  --
  -- >>> deterministicReversal (mk [4,2,6,4,3,1])
  -- [4,2,6,5,3,1],[5,6,2,4,3,1],[3,4,2,6,5,1],[2,4,3,6,5,1],[4,2,3,6,5,1],[6,3,2,4,5,1],[1,5,4,2,3,6]]
  deterministicReversals :: PP.Perm.Perm -> [PP.Perm.Perm]
  deterministicReversals = fmap PP.Perm.mkPerm . L.reverse . aux [] . PP.Perm.getList
    where
      aux acc []         = acc
      aux acc xs@(1 : _) = xs : acc
      aux acc xs@(i : _) = aux (xs : acc) (PP.Utils.List.prefixReversal i xs)

  -- |'longestDeterministicReversals' 'n'
  --
  -- >>> longestDeterministicReversals 4
  -- (4,[[3,1,4,2],[2,4,1,3]])
  -- >>> mapM_ (putStr . (\ s -> s ++ "\n") . show) . map deterministicReversal . T.snd $ longestDeterministicReversals 4
  -- [[3,1,4,2],[4,1,3,2],[2,3,1,4],[3,2,1,4],[1,2,3,4]]
  -- [[2,4,1,3],[4,2,1,3],[3,1,2,4],[2,1,3,4],[1,2,3,4]]
  -- >>> longestDeterministicReversals 5
  -- (7,[[3,1,4,5,2]])
  -- >>> mapM_ (putStr . (\ s -> s ++ "\n") . show) . map deterministicReversal . T.snd $ longestDeterministicReversals 5
  -- [[3,1,4,5,2],[4,1,3,5,2],[5,3,1,4,2],[2,4,1,3,5],[4,2,1,3,5],[3,1,2,4,5],[2,1,3,4,5],[1,2,3,4,5]]
  -- >>> longestDeterministicReversals 6
  -- (10,[[4,5,6,2,1,3],[4,1,5,2,6,3],[4,1,6,5,2,3],[3,6,5,1,4,2],[5,6,4,1,3,2]])
  -- >>> mapM_ (putStr . (\ s -> s ++ "\n") . show) . map deterministicReversal . T.snd $ longestDeterministicReversals 6
  -- [[4,5,6,2,1,3],[2,6,5,4,1,3],[6,2,5,4,1,3],[3,1,4,5,2,6],[4,1,3,5,2,6],[5,3,1,4,2,6],[2,4,1,3,5,6],[4,2,1,3,5,6],[3,1,2,4,5,6],[2,1,3,4,5,6],[1,2,3,4,5,6]]
  -- [[4,1,5,2,6,3],[2,5,1,4,6,3],[5,2,1,4,6,3],[6,4,1,2,5,3],[3,5,2,1,4,6],[2,5,3,1,4,6],[5,2,3,1,4,6],[4,1,3,2,5,6],[2,3,1,4,5,6],[3,2,1,4,5,6],[1,2,3,4,5,6]]
  -- [[4,1,6,5,2,3],[5,6,1,4,2,3],[2,4,1,6,5,3],[4,2,1,6,5,3],[6,1,2,4,5,3],[3,5,4,2,1,6],[4,5,3,2,1,6],[2,3,5,4,1,6],[3,2,5,4,1,6],[5,2,3,4,1,6],[1,4,3,2,5,6]]
  -- [[3,6,5,1,4,2],[5,6,3,1,4,2],[4,1,3,6,5,2],[6,3,1,4,5,2],[2,5,4,1,3,6],[5,2,4,1,3,6],[3,1,4,2,5,6],[4,1,3,2,5,6],[2,3,1,4,5,6],[3,2,1,4,5,6],[1,2,3,4,5,6]]
  -- [[5,6,4,1,3,2],[3,1,4,6,5,2],[4,1,3,6,5,2],[6,3,1,4,5,2],[2,5,4,1,3,6],[5,2,4,1,3,6],[3,1,4,2,5,6],[4,1,3,2,5,6],[2,3,1,4,5,6],[3,2,1,4,5,6],[1,2,3,4,5,6]]
  longestDeterministicReversals :: Int -> (Int, [PP.Perm.Perm])
  longestDeterministicReversals = F.foldr' f (0, []) . fmap ((\ ps -> (L.length ps - 1, L.head ps)) . deterministicReversals) . PP.Perm.Generator.derangements
    where
      f (m, p) (maxSoFar, acc)
        | m > maxSoFar  = (m,         [p])
        | m == maxSoFar = (maxSoFar , p : acc)
        | otherwise     = (maxSoFar , acc)

  -- |'deterministicReversalRadius' 'n' return the radius
  --
  -- >>> deterministicReversalRadius 2
  -- 1
  -- >>> deterministicReversalRadius 3
  -- 2
  -- >>> deterministicReversalRadius 4
  -- 4
  -- >>> deterministicReversalRadius 5
  -- 7
  -- >>> deterministicReversalRadius 6
  -- 10
  deterministicReversalRadius :: Int -> Int
  deterministicReversalRadius = T.fst . longestDeterministicReversals
