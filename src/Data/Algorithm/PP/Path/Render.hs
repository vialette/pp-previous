module Data.Algorithm.PP.Path.Render where

import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point

class Render where
  lStepChar :: Char
  rStepChar :: Chat
  getRenderPoints :: Path a -> [PP.Geometry.Point.Point]
