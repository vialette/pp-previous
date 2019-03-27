module Data.Algorithm.PP.Path.Dyck.Label (
    labelLeftToRightDown
  ) where

import qualified Data.Foldable   as F
import qualified Data.Tuple      as T

import qualified Data.Algorithm.PP.Path      as PP.Path
import qualified Data.Algorithm.PP.Path.Dyck as PP.Path.Dyck
import qualified Data.Algorithm.PP.Path.Step as PP.Path.Step

{- | 'labelLeftToRightDown'

-}
labelLeftToRightDown :: PP.Path.Path -> [Int]
labelLeftToRightDown p = T.fst . F.foldr labelUpteps ([], []) . T.fst . F.foldr labelDownSteps ([], n) $ PP.Path.getSteps p
  where
    n :: Int
    n = PP.Path.Dyck.semiLen p

    labelUpteps :: (Step, Int) -> ([Int], [(Step, Int)]) -> ([Int], [(Step, Int)])
    labelUpteps (PP.Path.Step.UpStep,   _) (acc, (PP.Path.Step.DownStep, i) : stack) = (i : acc, stack)
    labelUpteps (PP.Path.Step.DownStep, i) (acc, stack)                              = (i : acc, (PP.Path.Step.DownStep, i) : stack)
    labelUpteps _                           _                                        = error "non Dyck path"


    labelDownSteps :: Step -> [(Step, Int)] -> [(Step, Int)]
    labelDownSteps PP.Path.Step.UpStep   (acc, i) = ((PP.Path.Step.UpStep,   0) : acc, i)
    labelDownSteps PP.Path.Step.DownStep (acc, i) = ((PP.Path.Step.DownStep, i) : acc, i-1)
    labelDownSteps _                     _        = error "non Dyck path"


