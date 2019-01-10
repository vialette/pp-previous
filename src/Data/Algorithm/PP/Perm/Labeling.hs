module Data.Algorithm.PP.Perm.Labeling (
  fixedPointsLabeling
) where

import qualified Data.Algorithm.PP.Geometry.Point  as PP.Geometry.Point
import qualified Data.Algorithm.PP.Perm.Statistics as PP.Perm.Statistics
import qualified Data.Algorithm.PP.Perm            as PP.Perm
import qualified Data.Algorithm.PP.Utils.Integer   as PP.Utils.Integer

-- |
fixedPointsLabeling :: PP.Perm.Perm -> Integer
fixedPointsLabeling = PP.Utils.Integer.encode . fmap PP.Geometry.Point.getY . PP.Perm.Statistics.fixedPoints
