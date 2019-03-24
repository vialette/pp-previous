module Data.Algorithm.PP.Perm.Complexity
(
  complexity
, complexityStat
, complexityAny
, complexityAnyStat

, maxComplexity
, maxComplexityStat
, maxComplexityStat'

, maxComplexity'
) where

import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T

import qualified Data.Algorithm.PP.Perm           as PP.Perm
import qualified Data.Algorithm.PP.Perm.Generator as PP.Perm.Generator
import qualified Data.Algorithm.PP.Utils.Foldable as PP.Utils.Foldable
import qualified Data.Algorithm.PP.Utils.List     as PP.Utils.List

{- | 'complexity' @k@ @p@ function returns the list of all permutations of length
@k@ that occur in the permutation @p@.

>>> complexity 1 $ mkPerm [1,4,2,5,3]
[[1]]
>>> complexity 2 $ mkPerm [1,4,2,5,3]
[[1,2],[2,1]]
>>> complexity 3 $ mkPerm [1,4,2,5,3]
[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2]]
>>> complexity 4 $ mkPerm [1,4,2,5,3]
[[1,2,4,3],[1,3,2,4],[1,3,4,2],[1,4,2,3],[3,1,4,2]]
>>> complexity 5 $ mkPerm [1,4,2,5,3]
[[1,4,2,5,3]]
-}
complexity :: Int -> PP.Perm.Perm -> [PP.Perm.Perm]
complexity k = PP.Utils.List.uniq . L.map PP.Perm.mk . PP.Utils.List.subsets k . PP.Perm.getList

{- | 'complexityStat' @n@ @p@ function returns the number of permutations of length
@k@ that occurs in permutation @p@.

>>> complexityStat 1 (mkPerm [1,4,2,5,3])
1
>>> complexityStat 2 (mkPerm [1,4,2,5,3])
2
>>> complexityStat 4 (mkPerm [1,4,2,5,3])
5
>>> complexityStat 4 (mkPerm [1,4,2,5,3])
5
>>> complexityStat 5 (mkPerm [1,4,2,5,3])
1
-}
complexityStat :: Int -> PP.Perm.Perm -> Int
complexityStat k = L.length . complexity k

complexityAny :: PP.Perm.Perm -> [PP.Perm.Perm]
complexityAny p = F.concat [complexity k p | k <- [1..PP.Perm.len p]]

complexityAnyStat :: PP.Perm.Perm -> Int
complexityAnyStat = L.length . complexityAny

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
maxComplexity k = maxComplexity' k . PP.Perm.Generator.perms

-- >>> Complexity.maxComplexity' 3 (Perm.perms 5)
-- (6,[[2,5,3,1,4],[4,1,3,5,2]])
-- >>> Complexity.maxComplexity' 3 (Perm.perms 5) == Complexity.maxComplexity 3 5
-- True
maxComplexity' :: (Foldable t) => Int -> t PP.Perm.Perm -> (Int, [PP.Perm.Perm])
maxComplexity' k = PP.Utils.Foldable.maximumsBy (complexityStat k)

-- | The 'maxComplexityStat' 'k' 'n' function returns
-- the maximum number of permutations of length 'k' a permutation of length 'n'
-- can contain.
--
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

pat :: Int -> (Int, [PP.Perm.Perm])
pat = PP.Utils.Foldable.maximumsBy complexityAnyStat . PP.Perm.Generator.perms
