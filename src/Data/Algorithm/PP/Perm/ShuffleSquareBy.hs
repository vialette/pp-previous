module Data.Algorithm.PP.Perm.ShuffleSquareBy
(
  -- * Searching
  shuffleSquareRootsBy
, shuffleSquareRootsByStat
, shuffleSquareRootsByMult

  -- * Testing
, shuffleSquareBy
, simpleShuffleSquareBy
, kShuffleSquareBy
, extremalShuffleSquareBy
, kShuffleSquareByFree
, shuffleSquareByFree

  -- * Generating
, shuffleSquaresBy
, nonShuffleSquaresBy

  -- * Sub-permutations
, subShuffleSquaresBy
)
where

  import qualified Control.Arrow  as A
  import qualified Data.Foldable  as F
  import qualified Data.List      as L
  import qualified Data.Tuple     as T

  import qualified Data.Algorithm.PP.Combi      as PP.Combi
  import qualified Data.Algorithm.PP.Perm       as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List

  -- |'shuffleSquareRootsBy' 'f' 'p' returns all square roots of the permutation 'p'
  -- according to the bijection 'f'.
  shuffleSquareRootsBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> [PP.Perm.Perm]
  shuffleSquareRootsBy f = PP.Utils.List.uniq . L.map proj1 . L.filter test . L.map trans . PP.Combi.balPartitions . PP.Perm.toList
    where
      trans = PP.Perm.mk A.*** (f . PP.Perm.mk)
      test  = T.uncurry (==)
      proj1 = T.fst

  -- |'shuffleSquareRootsByStat' 'f' 'p'
  shuffleSquareRootsByStat :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Int
  shuffleSquareRootsByStat f = L.length . shuffleSquareRootsBy f

  -- |Alias for 'shuffleSquareRootsByStat'.
  shuffleSquareRootsByMult :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Int
  shuffleSquareRootsByMult = shuffleSquareRootsByStat

  -- |'shuffleSquareBy' 'f' 'p' returns 'True' if the permutation 'p' is a
  -- shuffle-square according to the bijection 'f'.
  shuffleSquareBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Bool
  shuffleSquareBy f = not . L.null . shuffleSquareRootsBy f

  -- |'simpleShuffleSquareBy' 'f' 'p' returns 'True' if the permutations 'p' has
  -- exactly one square root according to the bijection 'f'.
  simpleShuffleSquareBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Bool
  simpleShuffleSquareBy f = go . shuffleSquareRootsBy f
    where
      go [p] = True
      go _   = False

  -- |'kShuffleSquareBy' 'f' 'k' 'p' returns 'True' if the permutations 'p' has
  -- exactly 'k' square roots according to the bijection 'f'.
  kShuffleSquareBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> Int -> PP.Perm.Perm -> Bool
  kShuffleSquareBy f k = (==) k . L.length . shuffleSquareRootsBy f

  extremalShuffleSquareBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Bool
  extremalShuffleSquareBy f p = False

  kShuffleSquareByFree :: (PP.Perm.Perm -> PP.Perm.Perm) -> Int -> PP.Perm.Perm -> Bool
  kShuffleSquareByFree f k p
    | odd k     = True
    | otherwise = F.all (not . shuffleSquareBy f) $ PP.Perm.permPatterns k p

  -- |'shuffleSquareByFree' 'f' 'p' retusn 'True' if the permutations 'p' does not
  -- contain any pattern of length at least 4 that is a shuffle square according
  -- to the bijection 'f'.
  shuffleSquareByFree :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Bool
  shuffleSquareByFree f p = F.and [kShuffleSquareByFree f k p | k <- [4,6..n]]
    where
      n = PP.Perm.len p

  -- |'shuffleSquaresBy' 'n' returns all shuffle-square permutations of length 'n'.
  shuffleSquaresBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> Int -> [PP.Perm.Perm]
  shuffleSquaresBy f n
    | odd n     = []
    | otherwise = L.filter (shuffleSquareBy f) $ PP.Perm.perms n

  -- |'nonShuffleSquaresBy' 'n' returns all non-shuffle-square permutations of length 'n'.
  nonShuffleSquaresBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> Int -> [PP.Perm.Perm]
  nonShuffleSquaresBy f n
    | odd n     = PP.Perm.perms n
    | otherwise = L.filter (not . shuffleSquareBy f) $ PP.Perm.perms n

  -- |'subShuffleSquaresBy' 'p' return the longest shuffle-square subpermutations
  -- of permutation 'p'.
  subShuffleSquaresBy :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> [PP.Perm.Perm]
  subShuffleSquaresBy f p = go $ PP.Perm.len p
    where
      go k
        | odd k     = go (k-1)
        | otherwise = if L.null sqs then go (k-2) else sqs
        where
          sqs  = L.filter (shuffleSquareBy f) $ PP.Perm.permPatterns k p
