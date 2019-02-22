module Data.Algorithm.PP.Perm.Labeling (
  fixedPointsLabeling
) where

import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point
import qualified Data.Algorithm.PP.Perm.Features  as PP.Perm.Features
import qualified Data.Algorithm.PP.Perm           as PP.Perm
import qualified Data.Algorithm.PP.Utils.Integer  as PP.Utils.Integer

-- |'fixedPointsLabeling' 'p'
--
-- >>>
fixedPointsLabeling :: PP.Perm.Perm -> Integer
fixedPointsLabeling = PP.Utils.Integer.encode . fmap PP.Geometry.Point.getY . PP.Perm.Features.fixedPoints
