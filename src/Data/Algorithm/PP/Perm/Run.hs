module Data.Algorithm.PP.Perm.Run
(
  ascendingRuns
)
where

  import qualified Control.Arrow as A
  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Tuple    as T

  import qualified Data.Algorithm.PP.Combi      as PP.Combi
  import qualified Data.Algorithm.PP.Perm       as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List

  ascendingRuns :: PP.Perm.Perm -> [PP.Perm.Pattern]
  ascendingRuns = PP.Perm.toList
