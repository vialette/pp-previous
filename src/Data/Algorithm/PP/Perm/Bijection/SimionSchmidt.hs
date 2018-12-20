module Data.Algorithm.PP.Perm.Bijection.SimionSchmidt
(
  simionSchmidt
)
where

  import Control.Applicative
  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Tuple    as T

  import qualified Data.Algorithm.PP.Geometry.Point  as PP.Geometry.Point
  import qualified Data.Algorithm.PP.Perm            as PP.Perm
  import qualified Data.Algorithm.PP.Perm.Statistics as PP.Perm.Statistics

  -- |'simionSchmidt' 'p'
  simionSchmidt :: PP.Perm.Perm -> PP.Perm.Perm
  simionSchmidt p = PP.Perm.fromPoints (ps ++ ps'')
    where
      ps   = PP.Perm.Statistics.leftToRightMinima p
      ps'  = L.filter (`F.notElem` ps) $ PP.Perm.getPoints p
      ps'' = uncurry PP.Geometry.Point.mk <$> L.zip xs ys
        where
          xs = fmap PP.Geometry.Point.getX ps'
          ys = PP.Geometry.Point.getY <$> L.sortBy cmpDescY ps'
            where
              cmpDescY p1 p2 = PP.Geometry.Point.getY p2 `compare` PP.Geometry.Point.getY p1
