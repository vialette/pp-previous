module Data.Algorithm.PP.Combi
(
  subsets
, partitions
, balPartitions
)
where

  import qualified Data.List  as L
  import qualified Data.Tuple as T

  import qualified Data.Algorithm.PP.Utils.List as PP.List

  -- Return all subsets of a list.
  subsets :: (Eq t, Num t) => t -> [a] -> [[a]]
  subsets 0 _        = [[]]
  subsets _ []       = []
  subsets k (x : xs) = [x : xs' | xs' <- subsets (k-1) xs] ++ subsets k xs

  -- 'partitions' 'xs' 'nl' 'nr' return all possible partitions of 'xs' into 'ns'
  -- and 'nr' elements.
  partitions :: Eq a => [a] -> Int -> Int -> [([a], [a])]
  partitions xs nl nr
    | nl + nr /= L.length xs = []
    | otherwise              = [(xs', xs L.\\ xs') | xs' <- subsets nl xs]

  -- 'balPartitions' 'xs' returns all partitions of 'xs' into 'n/2' and 'n/2'
  -- elements, where 'n' is the length of 'xs'.
  balPartitions :: Ord a => [a] -> [([a], [a])]
  balPartitions []       = []
  balPartitions (x : xs) = PP.List.uniq $ fmap f ps
    where
      f p = (x : T.fst p, T.snd p)
      ps  = partitions xs (k-1) k
      k   = 1 + L.length xs `div` 2
