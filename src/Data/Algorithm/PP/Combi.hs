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

  subsets :: (Eq t, Num t) => t -> [a] -> [[a]]
  subsets 0 _        = [[]]
  subsets _ []       = []
  subsets k (x : xs) = [x : xs' | xs' <- subsets (k-1) xs] ++ subsets k xs

  --partitions :: Perm -> Int -> Int -> [(Pattern, Pattern)]
  partitions xs nl nr
    | nl + nr /= L.length xs = []
    | otherwise              = [(xs', xs L.\\ xs') | xs' <- subsets nl xs]

  --balPartitions :: Perm -> [(Pattern, Pattern)]
  balPartitions []       = []
  balPartitions (x : xs) = PP.List.uniq $ fmap f ps
    where
      f p = (x : T.fst p, T.snd p)
      ps  = partitions xs (k-1) k
      k   = 1 + L.length xs `div` 2
