module Data.Algorithm.PP.Path.Generator (
    -- * Badic paths
      paths

    -- * Constrained paths
    , endHeightPaths
    , startUpStepPaths
    , exactStartUpStepPaths
    , endDownStepPaths
    , exactEndDownStepPaths
    , startUpStepEndDownStepPaths
    , exactStartUpStepEndDownStepPaths

    -- * Positive paths
    , returnPositivePaths
    , positivePaths

    -- * Negative paths
    , returnNegativePaths
    , negativePaths
  ) where

import qualified Data.Foldable as F

import qualified Data.Algorithm.PP.Path          as PP.Path
import qualified Data.Algorithm.PP.Path.Step     as PP.Path.Step
import qualified Data.Algorithm.PP.Combinatorics as PP.Combinatorics

{- |'paths' @n@ returns all paths of length @n@ that start at height 0.

>>> let n = 4 in mapM_ print [(i, paths i) | i <- [0..n]]
(0,[])
(1,[),(])
(2,[)),)(,(),((])
(3,[))),))(,)(),)((,()),()(,((),(((])
(4,[)))),)))(,))(),))((,)()),)()(,)((),)(((,())),())(,()(),()((,(()),(()(,(((),((((])
-}
paths :: Int -> [PP.Path.Path]
paths = fmap PP.Path.mk . aux
  where
    aux n
      | n < 0     = []
      | n == 0    = [[]]
      | otherwise = fmap (PP.Path.Step.DownStep :) ss ++ fmap (PP.Path.Step.UpStep :) ss
        where
          ss = aux (n-1)

{- | 'endHeightPaths @h@ @n@ generates all paths of length @n@ that start at 0 and end
at height @h@.

>>> let n = 4 in mapM_ print [(i, endHeightPaths i n) | i <- [-n,-n+1..n]]
(-4,[))))])
(-3,[])
(-2,[())),)()),))(),)))(])
(-1,[])
(0,[))((,)()(,)((),(()),()(),())(])
(1,[])
(2,[)(((,()((,(()(,((()])
(3,[])
(4,[((((])
-}
endHeightPaths :: Int -> Int -> [PP.Path.Path]
endHeightPaths h n
  | h < 0  = PP.Path.complement <$> endHeightPaths (-h) n
  | h > n  = []
  | h == n = [PP.Path.upSteps h]
  | otherwise = ps ++ ps'
    where
      ps  = (PP.Path.downStep <>) <$> endHeightPaths (h+1) (n-1)
      ps' = (PP.Path.upStep   <>) <$> endHeightPaths (h-1) (n-1)

{- | 'startUpStepPaths' @k@ @n@ generates all paths of length @n@ that start with at least @k@ up-steps

>>> let n = 4 in mapM_ print [(i, startUpStepPaths i n) | i <- [0..n]]
(0,[)))),)))(,))(),))((,)()),)()(,)((),)(((,())),())(,()(),()((,(()),(()(,(((),((((])
(1,[())),())(,()(),()((,(()),(()(,(((),((((])
(2,[(()),(()(,(((),((((])
(3,[(((),((((])
(4,[((((])
-}
startUpStepPaths :: Int -> Int -> [PP.Path.Path]
startUpStepPaths k n = fmap (p <>) $ paths (n-k)
  where
    p = PP.Path.upSteps k

{- | 'exactStartUpStepPaths' @k@ @n@ generates all paths of length @n@ that start with exactly @k@ up-steps

>>> let n = 4 in mapM_ print [(i, exactStartUpStepPaths i n) | i <- [0..n]]
(0,[))))),))))(,)))(),)))((,))()),))()(,))((),))(((,)())),)())(,)()(),)()((,)(()),)(()(,)(((),)((((])
(1,[()))),()))(,())(),())((,()()),()()(,()((),()(((])
(2,[(())),(())(,(()(),(()((])
(3,[((()),((()(])
(4,[(((()])
-}
exactStartUpStepPaths :: Int -> Int -> [PP.Path.Path]
exactStartUpStepPaths k n = fmap (p <>) $ paths (n-k)
  where
    p = PP.Path.upSteps k <> PP.Path.downStep

{- | 'endDownStepPaths' @k@ @n@ generates all paths of length @n@ that end with at least  @k@ down-steps.

>>> let n = 4 in mapM_ print [(i, endDownStepPaths i n) | i <- [0..n]]
(0,[((((,)(((,()((,))((,(()(,)()(,())(,)))(,(((),)((),()(),))(),(()),)()),())),))))])
(1,[(((),)((),()(),))(),(()),)()),())),))))])
(2,[(()),)()),())),))))])
(3,[())),))))])
(4,[))))])
-}
endDownStepPaths :: Int -> Int -> [PP.Path.Path]
endDownStepPaths k = fmap PP.Path.rev . startUpStepPaths k

{- | 'exactEndDownStepPaths' @k@ @n@ generates all paths of length @n@ that end with at least  @k@ down-steps.

>>> let n = 4 in mapM_ print [(i, exactEndDownStepPaths i n) | i <- [0..n]]
(0,[(((((,)((((,()(((,))(((,(()((,)()((,())((,)))((,((()(,)(()(,()()(,))()(,(())(,)())(,()))(,))))(])
(1,[((((),)(((),()((),))((),(()(),)()(),())(),)))()])
(2,[((()),)(()),()()),))())])
(3,[(())),)()))])
(4,[())))])
-}
exactEndDownStepPaths :: Int -> Int -> [PP.Path.Path]
exactEndDownStepPaths k = fmap PP.Path.rev . exactStartUpStepPaths k

{- | 'startUpStepEndDownStepPaths' @k@ @l@ @n@ generates all paths of length @n@ that start with at least @k@ up-steps
and end with at least @l@ down-steps.

>>> let n = 4 in mapM_ print [(i, j, startUpStepEndDownStepPaths i j n) | i <- [0..n], j <- [0..n], i+j <= n]
(0,0,[)))),)))(,))(),))((,)()),)()(,)((),)(((,())),())(,()(),()((,(()),(()(,(((),((((])
(0,1,[)))),))(),)()),)((),())),()(),(()),((()])
(0,2,[)))),)()),())),(())])
(0,3,[)))),()))])
(0,4,[))))])
(1,0,[())),())(,()(),()((,(()),(()(,(((),((((])
(1,1,[())),()(),(()),((()])
(1,2,[())),(())])
(1,3,[()))])
(2,0,[(()),(()(,(((),((((])
(2,1,[(()),((()])
(2,2,[(())])
(3,0,[(((),((((])
(3,1,[((()])
(4,0,[((((])
-}
startUpStepEndDownStepPaths :: Int -> Int -> Int -> [PP.Path.Path]
startUpStepEndDownStepPaths k l n = [p' <> p <> p'' | p <- paths (n-k-l)]
  where
    p'  = PP.Path.upSteps k
    p'' = PP.Path.downSteps l

{- | 'exactStartUpStepEndDownStepPaths' @k@ @l@ @n@ generates all paths of length @n@ that start with exactly @k@ up-steps
and end with exactly @l@ down-steps.

>>> let n = 4 in mapM_ print [(i, j, exactStartUpStepEndDownStepPaths i j n) | i <- [0..n], j <- [0..n], i+j <= n]
(0,0,[)))(,))((,)()(,)(((])
(0,1,[))(),)(()])
(0,2,[)())])
(0,3,[])
(0,4,[])
(1,0,[())(,()((])
(1,1,[()()])
(1,2,[])
(1,3,[])
(2,0,[(()(])
(2,1,[])
(2,2,[])
(3,0,[])
(3,1,[])
(4,0,[])
-}
exactStartUpStepEndDownStepPaths :: Int -> Int -> Int -> [PP.Path.Path]
exactStartUpStepEndDownStepPaths k l n = [p' <> p <> p'' | p <- paths (n-k-l-2)]
  where
    p'  = PP.Path.upSteps k <> PP.Path.downStep
    p'' = PP.Path.upStep    <> PP.Path.downSteps l

{- | 'returnPositivePaths' @k@ @n@ returns all positive paths of length @n@ with @k@ internal returns to 0 and
that end at 0.

>>> returnPositivePaths 0 6
[(()()),((()))]
>>> returnPositivePaths 1 6
[()(()),(())()]
>>> returnPositivePaths 2 6
[()()()]
>>> returnPositivePaths 3 6
[]
-}
returnPositivePaths :: Int -> Int -> [PP.Path.Path]
returnPositivePaths k n = F.concatMap f $ PP.Combinatorics.evenPartitions (k+1) n
    where
      f :: [Int] -> [PP.Path.Path]
      f xs = (PP.Path.mk . mconcat) <$> sequence [[[PP.Path.Step.UpStep] `mappend` ss `mappend` [PP.Path.Step.DownStep] | ss <- aux (x-2)] | x <- xs]

      aux :: Int-> [[PP.Path.Step.Step]]
      aux 0  = [[]]
      aux n' = [[PP.Path.Step.UpStep] `mappend` ss `mappend` [PP.Path.Step.DownStep] `mappend` ss' | m <- [0..n'-2]
                                                                         , ss  <- aux m
                                                                         , ss' <- aux (n'-2-m)]


{- | 'positivePaths' @n@ generates all positive paths of length @n@ that start and end at 0.

-}
positivePaths :: Int -> [PP.Path.Path]
positivePaths n = F.concat [returnPositivePaths k n | k <- [0..n `div` 2]]

{- | 'returnNegativePaths @k@ @n@ returns all negative paths of length @n@ with @k@ internal returns to 0 and
that end at 0.
-}
returnNegativePaths :: Int -> Int -> [PP.Path.Path]
returnNegativePaths k n = PP.Path.complement <$> returnPositivePaths k n

{- | 'negativePaths' @n@ generates all negative paths of length @n@ that start and end at 0.

-}
negativePaths :: Int -> [PP.Path.Path]
negativePaths n = PP.Path.complement <$> positivePaths n
