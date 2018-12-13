module Data.Algorithm.PP.Perm.Complexity
(
  complexity
, complexityStat

, maxComplexity
, maxComplexityStat
, maxComplexityStat'

, maxComplexity'
)
where

  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Tuple    as T

  import qualified Data.Algorithm.PP.Combi      as PP.Combi
  import qualified Data.Algorithm.PP.Perm       as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List as PP.List

  -- |The 'complexity' 'k' 'p' function returns the list of all permutations of length
  -- 'k' that occur in the permutation 'p'.
  --
  -- >>> complexity 1 (mk [1,4,2,5,3])
  -- [[1]]
  -- >>> complexity 2 (mk [1,4,2,5,3])
  -- [[1,2],[2,1]]
  -- >>> complexity 3 (mk [1,4,2,5,3])
  -- [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2]]
  -- >>> complexity 4 (mk [1,4,2,5,3])
  -- [[1,2,4,3],[1,3,2,4],[1,3,4,2],[1,4,2,3],[3,1,4,2]]
  -- >>> complexity 5 (mk [1,4,2,5,3])
  -- [[1,4,2,5,3]]
  complexity :: Int -> PP.Perm.Perm -> [PP.Perm.Perm]
  complexity k = PP.List.uniq . L.map PP.Perm.mkPerm . PP.Combi.subsets k . PP.Perm.getList

  -- |The 'complexityStat' 'n' 'p' function returns the number of permutations of length
  -- 'k' that occurs in permutation 'p'.
  --
  -- >>> import qualified Data.Algorithm.PP.Perm as Perm
  -- >>> import qualified Data.Algorithm.PP.Perm.Complexity as Complexity
  -- >>>
  -- >>> complexityStat 1 (mk [1,4,2,5,3])
  -- 1
  -- >>> complexityStat 2 (mk [1,4,2,5,3])
  -- 2
  -- >>> complexityStat 4 (mk [1,4,2,5,3])
  -- 5
  -- >>> complexityStat 4 (mk [1,4,2,5,3])
  -- 5
  -- >>> complexityStat 5 (mk [1,4,2,5,3])
  -- 1
  complexityStat :: Int -> PP.Perm.Perm -> Int
  complexityStat k = L.length . complexity k

  -- | The 'maxComplexity' 'k' 'n' function returns a pair ('m', 'ps'), where 'm' is
  -- the maximum number of permutations of length 'k' a permutation of length 'n'
  -- can contain and 'ps' is the list of all permutations of length 'k' that
  -- contain 'm' distinct permutations of length 'k'.
  --
  -- >>> maxComplexity 1 3
  -- (1,[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]])
  -- >>> maxComplexity 2 3
  -- (2,[[1,3,2],[2,1,3],[2,3,1],[3,1,2]])
  -- >>> maxComplexity 3 3
  -- (1,[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]])
  maxComplexity :: Int -> Int -> (Int, [PP.Perm.Perm])
  maxComplexity k = maxComplexity' k . PP.Perm.perms

  -- >>> Complexity.maxComplexity' 3 (Perm.perms 5)
  -- (6,[[2,5,3,1,4],[4,1,3,5,2]])
  -- >>> Complexity.maxComplexity' 3 (Perm.perms 5) == Complexity.maxComplexity 3 5
  -- True
  maxComplexity' :: Int -> [PP.Perm.Perm] -> (Int, [PP.Perm.Perm])
  maxComplexity' k = F.foldr f (0, [])
    where
      f p (maxSoFar, acc)
        | k > maxSoFar  = (k, [p])
        | k == maxSoFar = (maxSoFar, p : acc)
        | otherwise     = (maxSoFar, acc)
          where
            k = complexity k p

  -- | The 'maxComplexityStat' 'k' 'n' function returns
  -- the maximum number of permutations of length 'k' a permutation of length 'n'
  -- can contain.
  --
  -- >>> import qualified Data.Algorithm.PP.Perm as Perm
  -- >>> import qualified Data.Algorithm.PP.Perm.Complexity as Complexity
  -- >>>
  -- >>> maxComplexityStat 1 3
  -- 1
  -- >>> maxComplexityStat 2 3
  -- 2
  -- >>> maxComplexityStat 3 3
  -- 1
  maxComplexityStat :: Int -> Int -> Int
  maxComplexityStat k = T.fst . maxComplexity k

  -- | The 'maxComplexityStat'' 'k' 'n' function returns
  -- the the list of all permutations of length 'n' that contain a maximum number
  -- of permutations of length 'k'.
  --
  -- >>> import qualified Data.Algorithm.PP.Perm as Perm
  -- >>> import qualified Data.Algorithm.PP.Perm.Complexity as Complexity
  -- >>>
  -- >>> maxComplexityStat' 1 3
  -- [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
  -- >>> maxComplexityStat' 2 3
  -- [[1,3,2],[2,1,3],[2,3,1],[3,1,2]]
  -- >>> maxComplexityStat' 3 3
  -- [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
  maxComplexityStat' :: Int -> Int -> [PP.Perm.Perm]
  maxComplexityStat' k = T.snd . maxComplexity k

  -- |The 'square' function squares an integer.
  maxHalfComplexity :: Int -> (Int, [PP.Perm.Perm])
  maxHalfComplexity n = maxComplexity (n `div` 2) n
