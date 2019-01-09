module Data.Algorithm.PP.Perm.Pattern
(
  superPatterns
)
where

  import qualified Control.Arrow   as Arrow
  import qualified Data.Foldable   as F
  import Data.Function (on)
  import qualified Data.List       as L
  import qualified Data.Tuple      as T

  import qualified Data.Algorithm.PP.Perm                 as PP.Perm
  import qualified Data.Algorithm.PP.Perm.Generator.Basic as PP.Perm.Generator.Basic
  import qualified Data.Algorithm.PP.Perm.Complexity      as PP.Perm.Complexity

  -- superPatterns :: Int -> Int -> [PP.Perm.Perm]
  superPatterns k = F.foldr' f (0, []) . PP.Perm.Generator.Basic.perms
    where
      f p (maxSoFar, acc)
        | m > maxSoFar  = (m, [p])
        | m == maxSoFar = (maxSoFar, p : acc)
        | otherwise     = (maxSoFar, acc)
          where
            m = PP.Perm.Complexity.complexityStat k p
