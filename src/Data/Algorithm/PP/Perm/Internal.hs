module Data.Algorithm.PP.Perm.Internal
(
  -- * Type
  Perm
, Patt
, FPerm

  -- * Building
, mkPerm
, mkPatt

  -- * Transforming
, getPoints
, getList
, getCoords

  -- * Querying
, len

, prefix
, prefixes
, suffix
, suffixes
, factors
, factors'
, maxfactors
, patterns
, patterns'
, maxPatterns

, partitions

, inversions
)
where

  import qualified Control.Arrow as A
  import qualified Data.Foldable as F
  import qualified Data.List     as L

  import Data.Algorithm.PP.Perm.Internal.Type
  import qualified Data.Algorithm.PP.Combi          as PP.Combi
  import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point
  import qualified Data.Algorithm.PP.Utils.List     as PP.Utils.List

  -- |'mkPerm' 'ps'
  mkPerm :: [PP.Geometry.Point.Point] -> Perm
  mkPerm = P

  -- |'mkPatt' 'ps'
  mkPatt :: [PP.Geometry.Point.Point] -> Patt
  mkPatt = P

  -- | 'getList' 'p' returns the list of the elements of permutation 'p'.
  getList :: P a -> [PP.Geometry.Point.Y]
  getList = L.map PP.Geometry.Point.getY . getPoints

  -- | 'coords' 'p' returns the permutation 'p' as pairs of integers.
  --
  -- >>> coords (mk [1,3,4,2])
  -- [(1,1),(2,3),(3,4),(4,2)]
  --- >>> :type coords (mk [1,3,4,2])
  -- coords (mk [1,3,4,2]) :: [(Data.Algorithm.PP.Perm.X, Data.Algorithm.PP.Perm.Y)]
  getCoords :: P a -> [(PP.Geometry.Point.X, PP.Geometry.Point.Y)]
  getCoords = L.map PP.Geometry.Point.getCoords . getPoints

  -- | 'len' 'p' returns the length of the permutation 'p'.
  len :: P a -> Int
  len = L.length . getPoints

  -- 'prefix' 'k' 'p'
  --
  -- >>> p = mk [2,4,5,1,6,3]
  -- >>> [prefix i p | i <- [1..len p]]
  -- [[1],[1,2],[1,2,3],[2,3,4,1],[2,3,4,1,5],[2,4,5,1,6,3]]
  prefix :: Int -> P a -> Patt
  prefix k = P . L.take k . getPoints

  -- |'prefixes' 'p' returns all the prefixes
  --
  -- >>> p = mk [2,4,5,1,6,3]
  -- >>> prefixes p
  -- [[1],[1,2],[1,2,3],[2,3,4,1],[2,3,4,1,5],[2,4,5,1,6,3]]
  prefixes :: P a -> [Patt]
  prefixes = L.map P . L.tail . L.inits . getPoints

  -- 'suffix' 'k' 'p' returns the suffix of length 'k' of the permutation 'p' as
  -- a permutation.
  --
  -- >>> p = mk [2,4,5,1,6,3]
  -- >>> [suffix i p | i <- [1..len p]]
  -- [[1],[2,1],[1,3,2],[3,1,4,2],[3,4,1,5,2],[2,4,5,1,6,3]]
  suffix :: Int -> P a -> Patt
  suffix k p = P . L.drop (n-k) $ getPoints p
    where
      n = len p

  -- |'suffixes' 'p' returns all the suffixes of the permutation 'p' as
  -- permutations.
  --
  -- >>> p = mk [2,4,5,1,6,3]
  -- >>> suffixes p
  -- [[2,4,5,1,6,3],[3,4,1,5,2],[3,1,4,2],[1,3,2],[2,1],[1]]
  suffixes :: P a -> [Patt]
  suffixes = L.map P . L.init . L.tails . getPoints

  -- |'factors' 'k' 'p' returns the list of all factors of length 'k' of the
  -- permutation 'p'.
  --
  -- >>> p = mk [2,4,1,3]
  -- >>> factors 0 p
  -- []
  -- >>> factors 1 p
  -- [[1],[2],[3],[4]]
  -- >>> factors 2 p
  -- [[1,3],[2,4],[4,1]]
  -- >>> factors 3 p
  -- [[2,4,1],[4,1,3]]
  -- >>> factors 4 p
  -- [[2,4,1,3]]
  -- >>> factors 5 p
  -- []
  factors :: Int -> P a -> [Patt]
  factors k = L.map P . PP.Utils.List.chunk k . getPoints

  factors' :: P a -> [Patt]
  factors' p = F.concat [factors k p |  k <- [1..len p]]

  maxfactors :: (Patt -> Bool) -> P a -> [Patt]
  maxfactors f p = select $ L.dropWhile L.null [[q | q <- factors k p, f q] | k <- [n,n-1..1]]
    where
      n         = len p
      select xs = if L.null xs then [] else L.head xs

  -- | 'patterns' 'k' 'p' returns all distinct permutations of length 'k' that
  -- occurs in permutation 'p'.
  --
  -- >>> patterns 0 (mk [2,4,1,3,5])
  -- [[]]
  -- >>> patterns 1 (mk [2,4,1,3,5])
  -- [[1]]
  -- >>> patterns 2 (mk [2,4,1,3,5])
  -- [[1,2],[2,1]]
  -- >>> patterns 3 (mk [2,4,1,3,5])
  -- [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2]]
  -- >>> patterns 4 (mk [2,4,1,3,5])
  -- [[1,3,2,4],[2,1,3,4],[2,3,1,4],[2,4,1,3],[3,1,2,4]]
  -- >>> patterns 5 (mk [2,4,1,3,5])
  -- [[2,4,1,3,5]]
  -- >>> patterns 6 (mk [2,4,1,3,5])
  -- []
  patterns :: Int -> P a -> [Perm]
  patterns k = L.map mkPerm . PP.Combi.subsets k . getPoints

  -- |'patterns'' 'k' 'p'
  patterns' :: Int -> P a -> [Perm]
  patterns' k = L.map (mkPerm . getPoints) . patterns k

  -- |'maxPatterns' 'f' 'p' returns the longest patterns 'q' of permutation 'p'
  -- such that 'f' 'q' holds.
  maxPatterns :: (Perm-> Bool) -> P a -> [Perm]
  maxPatterns f p = select $ L.dropWhile L.null [[q | q <- patterns k p, f q] | k <- [n,n-1..1]]
    where
      n         = len p
      select ys = if L.null ys then [] else L.head ys

  -- |'partitions' 'p' 'k' 'l' returns all partitions ('qk','ql') of the permutation
  -- 'p' such that '|qk|=k' and '|ql|=l'
  --
  -- >>>
  partitions :: Int -> Int -> P a -> [(Patt, Patt)]
  partitions k l = L.map ((A.***) mkPatt mkPatt) . PP.Combi.partitions k l . getPoints

  -- |'inversions' 'p' returns the inversions of the permutation 'p'.
  --
  -- >>> inversions (mk [1,5,3,2,6,4])
  -- [(5,3),(5,2),(5,4),(3,2),(6,4)]
  inversions :: P a -> [(PP.Geometry.Point.Y, PP.Geometry.Point.Y)]
  inversions = L.map (\[i, j] -> (i, j)) . L.filter (\[i, j] -> i > j) . PP.Combi.subsets 2 . getList
