module Data.Algorithm.PP.Path.Step.Render.UpDiagonal (
    getRenderPoints
  , lStepChar
  , rStepChar
  ) where

import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point
import           Data.Algorithm.PP.Path.Internal
import qualified Data.Algorithm.PP.Path.Step      as PP.Path.Step

-- |'getRenderPoints' 'p'
getRenderPoints :: Path a -> [PP.Geometry.Point.Point]
getRenderPoints = L.reverse . T.snd . F.foldl f (PP.Geometry.Point.zero, []) . getSteps
    where
      f (p, acc) (PP.Path.Step.LStep _) = (p', PP.Path.Step.LStep p' : acc)
        where
          p' = PP.Geometry.Point.mv (+1) (+1) p
      f (p, acc) (PP.Path.Step.RStep _) = ((p', PP.Path.Step.LStep p'' : acc)
        where
          p'  = PP.Geometry.Point.mv (+1) (-1) p
          p'' = PP.Geometry.Point.mv (+1) 0    p

-- Default LStep character.
lStepChar :: Char
lStepChar = '|'

-- Default RStep character.
rStepChar :: Char
rStepChar = '-'
