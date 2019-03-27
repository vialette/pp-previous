module Data.Algorithm.PP.Path.DyckPath.Label (
    labelLeftToRightDown
  ) where

import qualified Data.Foldable   as F
import qualified Data.Tuple      as T

import qualified Data.Algorithm.PP.Path      as PP.Path
import qualified Data.Algorithm.PP.Path.DyckPath as PP.Path.Dyck
import qualified Data.Algorithm.PP.Path.Step as PP.Path.Step

type AnnotatedStep a = (PP.Path.Step.Step, a)

{- | 'labelLeftToRightDown' @p@

-}
labelLeftToRightDown :: PP.Path.Path -> [Int]
labelLeftToRightDown p = T.fst . F.foldr labelUpteps ([], []) . T.fst . F.foldr labelDownSteps ([], n) $ PP.Path.getSteps p
  where
    n :: Int
    n = PP.Path.Dyck.semiLen p

    labelUpteps :: (PP.Path.Step.Step, a) -> ([a], [AnnotatedStep a]) -> ([a], [AnnotatedStep a])
    labelUpteps (PP.Path.Step.UpStep,   _) (acc, (PP.Path.Step.DownStep, i) : stack) = (i : acc, stack)
    labelUpteps (PP.Path.Step.DownStep, i) (acc, stack)                              = (i : acc, (PP.Path.Step.DownStep, i) : stack)
    labelUpteps _                           _                                        = error "non Dyck path"

    labelDownSteps :: Num a => PP.Path.Step.Step -> ([AnnotatedStep a], a) -> ([AnnotatedStep a], a)
    labelDownSteps PP.Path.Step.UpStep   (acc, i) = ((PP.Path.Step.UpStep,   0) : acc, i)
    labelDownSteps PP.Path.Step.DownStep (acc, i) = ((PP.Path.Step.DownStep, i) : acc, i-1)


