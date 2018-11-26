module Data.Algorithm.PP.Perm.Run
(
  ascendingRuns
)
where

  import qualified Control.Arrow as A
  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.List.Split as L.Split
  import qualified Data.Tuple    as T

  import qualified Data.Algorithm.PP.Combi      as PP.Combi
  import qualified Data.Algorithm.PP.Perm       as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List

  --ascendingRuns :: PP.Perm.Perm -> [PP.Perm.Pattern]
  ascendingRuns = aux [] . PP.Utils.List.chunk2 . PP.Perm.toList
    where
      aux acc [] = L.reverse
      aux [] [(y, y')] = aux [y, y']
      aux [] ((y, _) : ys) = aux [y] ys
      aux
