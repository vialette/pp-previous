module Data.Algorithm.PP.Perm.Run
(
  ascendingRuns
)
where

  import qualified Control.Arrow as A
  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Tuple    as T

  import Data.Algorithm.PP.Geometry.Point ((@<|))
  import qualified Data.Algorithm.PP.Perm       as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List

  -- |'ascendingRuns' 'p'
  ascendingRuns :: PP.Perm.Perm -> [PP.Perm.Pattern]
  ascendingRuns = L.map PP.Perm.fromPointsUnsafe . PP.Utils.List.groupBy' (@<|) . PP.Perm.getPoints
