module Data.Algorithm.PP.Complexity
(
  complexity
, maxComplexity
, maxHComplexity
)
where

  import qualified Data.List as L

  import qualified Data.Algorithm.PP.Combi      as PP.Combi
  import qualified Data.Algorithm.PP.Perm       as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List as PP.List

  type K = Int

  --
  complexity :: K -> PP.Perm.Perm -> Int
  complexity k = L.length . PP.List.uniq . PP.Combi.subsets k . PP.Perm.toList

  --
  maxComplexity :: K -> Int -> (Int, [PP.Perm.Perm])
  maxComplexity k = go 0 [] . PP.Perm.perms
    where
      go m acc []       = (m, L.sort acc)
      go m acc (p : ps) = aux (complexity p)
        where
          aux m'
            | m' > m    = go m' [p]       ps
            | m' == m   = go m  (p : acc) ps
            | otherwise = go m  acc       ps

  --
  maxHComplexity :: Int -> (Int, [PP.Perm.Perm])
  maxHComplexity n = maxComplexity (n `div` 2) n
