module Data.Algorithm.PP.Path (
    -- * Types
    Path(..)
  , Render(..)

    -- *
  , module Data.Algorithm.PP.Path.Step

    -- * Constructing
  , mk
  , paths
  , returnPaths
  , noReturnPaths
  , returnPosPaths
  , anyReturnPosPaths
  , returnNegPaths
  , anyReturnNegPaths
  , anyReturnPaths
  , startLStepPaths
  , startRStepPaths
  , strictlyPosPaths
  , posPaths
  , strictlyNegPaths
  , negPaths

    -- * Transforming
  , mirror
  , rev
  , splitAtReturn

    -- * Seeing
  , showWithParameter

    -- * Querying
  , len
  , semiLen

    -- * Drawing
  , draw
  , draw'
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T
import Prelude hiding (map)

import Data.Algorithm.PP.Path.Internal
import qualified Data.Algorithm.PP.Path.Step      as PP.Path.Step
import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point

data Render = Horizontal | Diagonal

instance Semigroup (Path a) where
  p <> p' = mk (getSteps p ++ getSteps p')

instance Monoid (Path a) where
    mempty = mk []
    mappend = (<>)

instance (Show a) => Show (Path a) where
  show = F.concatMap show . getSteps

-- |'showWithParameter' 'p'
showWithParameter :: (Show a) => Path a -> String
showWithParameter = F.concatMap f . getSteps
  where
    f (PP.Path.Step.LStep x)    = '(' : show x
    f (PP.Path.Step.RStep x)  = ')' : show x

-- |'mk' 'ss' return a path from a list of steps 'ss'.
--
-- >>>
mk :: [PP.Path.Step.Step a] -> Path a
mk ss = Path { getSteps = ss }

-- |'len' 'path' return the length of the path 'path'.
len :: Path a -> Int
len = L.length . getSteps

-- |'semiLen' 'path' return the semi-length of the path 'path'.
semiLen :: Path a -> Int
semiLen = flip div 2 . len

-- |'LSteps' 'n' returns the path composed of 'n' up-steps.
LSteps :: Int -> Path ()
LSteps = mk . flip L.replicate (PP.Path.Step.LStep ())

-- |'RSteps' 'n' returns the path composed of 'n' down-steps.
RSteps :: Int -> Path ()
RSteps = mk . flip L.replicate (PP.Path.Step.RStep ())

-- |'rev' 'p' reverses the path 'p'.
--
-- >>>
rev :: Path a -> Path a
rev = mk . L.reverse . fmap PP.Path.Step.flipStep . getSteps

-- |'mirror' 'p' returns the mirror of the path 'p'.
--
-- >>>
mirror :: Path a -> Path a
mirror = mk . fmap PP.Path.Step.flipStep . getSteps

-- |'map' 'f' 'p'
--
-- >>>
map :: (PP.Path.Step.Step a -> Step b) -> Path a -> Path b
map f = mk . fmap f . getSteps

-- |'dropParameter' 'p'
dropParameter :: Path a -> Path ()
dropParameter = map PP.Path.Step.dropParameter

-- |'paths' 'n' returns all paths of length 'n' starting from the x-axis.
--
-- >>>
paths :: Int -> [Path ()]
paths 0 = []
paths n = mk <$> aux n
  where
    aux :: Int -> [[Step ()]]
    aux 0 = [[]]
    aux n' = (:) <$> [PP.Path.Step.LStep (), PP.Path.Step.RStep ()] <*> aux (n'-1)

-- Auxialiary function
predicatePosPaths :: (Int -> Bool) -> Int -> [Path ()]
predicatePosPaths p n
  | n <= 0    = []
  | otherwise = mk <$> aux n 0
    where
      aux :: Int -> Int -> [[Step ()]]
      aux 0  _ = [[]]
      aux n' h
        | p h       = PP.Path.Step.LSteps
        | otherwise = PP.Path.Step.LSteps `mappend` PP.Path.Step.RSteps
        where
          PP.Path.Step.LSteps :: [[Step ()]]
          PP.Path.Step.LSteps   = (:) <$> [PP.Path.Step.LStep ()]   <*> aux (n'-1) (h+1)

          PP.Path.Step.RSteps :: [[Step ()]]
          PP.Path.Step.RSteps = (:) <$> [PP.Path.Step.RStep ()] <*> aux (n'-1) (h-1)

-- | 'strictlyPosPaths' 'n' returns all strictly positive paths of length 'n'
-- starting from the x-axis with an up-step .
--
-- >>>
strictlyPosPaths :: Int -> [Path ()]
strictlyPosPaths = predicatePosPaths (<= 1)

-- | 'posPaths' 'n' returns all positive paths of length 'n' starting from the
-- x-axis with an up-step .
--
-- >>>
posPaths :: Int -> [Path ()]
posPaths = predicatePosPaths (== 1)

-- |'returnPosPaths' 'k' 'n' return all positive paths of length 'n' starting from and
-- ending at the x-axis with 'k' internal returns to the x-axis.
--
-- >>>
returnPosPaths :: Int -> Int -> [Path ()]
returnPosPaths k n = F.concatMap f $ evenPartitions (k+1) n
    where
      f :: [Int] -> [Path ()]
      f xs = (mk . mconcat) <$> sequence [[[PP.Path.Step.LStep ()] `mappend` ss `mappend` [PP.Path.Step.RStep ()] | ss <- aux (x-2)] | x <- xs]

      aux :: Int-> [[Step ()]]
      aux 0  = [[]]
      aux n' = [[PP.Path.Step.LStep ()] `mappend` ss `mappend` [PP.Path.Step.RStep ()] `mappend` ss' | m <- [0..n'-2]
                                                                               , ss  <- aux m
                                                                               , ss' <- aux (n'-2-m)]

-- |'anyReturnPosPaths' 'n' return all positive paths of length 'n' starting from and
-- ending at the x-axis with no constraint on the number of internal returns to the x-axis.
--
-- >>>
anyReturnPosPaths :: Int -> [Path ()]
anyReturnPosPaths n = F.concat [returnPosPaths k n | k <- [0..n `div`2]]

-- | 'strictlyNegPaths' 'n' returns all strictly negative paths of length 'n'
-- starting from the x-axis with an up-step .
--
-- >>>
strictlyNegPaths :: Int -> [Path ()]
strictlyNegPaths = fmap mirror . strictlyNegPaths

-- | 'negPaths' 'n' returns all negative paths of length 'n' starting from the
-- x-axis with an up-step .
--
-- >>>
negPaths :: Int -> [Path ()]
negPaths = fmap mirror . posPaths

-- |'returnNegPaths' 'k' 'n' return all negative paths of length 'n' starting from and
-- ending at the x-axis with 'k' internal returns to the x-axis.
--
-- >>>
returnNegPaths :: Int -> Int -> [Path ()]
returnNegPaths k = fmap mirror . returnPosPaths k

-- |'anyReturnNegPaths' 'n' return all negative paths of length 'n' starting from and
-- ending at the x-axis with no constraint on the number of internal returns to the x-axis.
--
-- >>>
anyReturnNegPaths :: Int -> [Path ()]
anyReturnNegPaths = fmap mirror . anyReturnPosPaths

-- |'anyReturnPaths' 'n' returns all paths of length 'n' that start and terminate at
-- the x-axis.
anyReturnPaths :: Int -> [Path ()]
anyReturnPaths = F.concatMap doPath . anyReturnPosPaths
  where
    doPath :: Path () -> [Path ()]
    doPath = F.concat . foldr combine [[]] . splitAtReturn
      where
        combine :: Path () -> [[Path ()]] -> [[Path ()]]
        combine p acc = withPosPath p acc ++ withNegPath p acc)
          where
            withPosPath p acc = fmap (p :) acc
            withNegPath p acc = fmap (mirror p :) acc

-- |'startLStepPaths' 'k' 'n' returns all paths of length 'n' that start from
-- the x-axis with 'k' up-steps.
--
-- >>>
startLStepPaths :: Int -> Int -> [Path ()]
startLStepPaths k n = (PP.Path.Step.LSteps k <>) <$> paths (n-k)

-- |'startRStepPaths' 'k' 'n' returns all paths of length 'n' that start from
-- the x-axis with 'k' down-steps.
--
-- >>>
startRStepPaths :: Int -> Int -> [Path ()]
startRStepPaths k = fmap mirror . startLStepPaths k

-- |'endPP.Path.Step.startLStepPaths' 'k' 'n' returns all paths of length 'n' that end at
-- the x-axis with 'k' up-steps.
--
-- >>>
endLStepPaths:: Int -> Int -> [Path ()]
endLStepPathsk = fmap rev . startRStepPaths k

-- |'endPP.Path.Step.startRStepPaths' 'k' 'n' returns all paths of length 'n' that end at
-- the x-axis with 'k' down-steps.
--
-- >>>
endRStepPaths:: Int -> Int -> [Path ()]
endRStepPathsk = fmap rev . endLStepPathsk

-- |'endHeightPaths' 'h' 'n' returns all paths of length 'n' that terminate at
-- height 'h'.
--
-- >>>
endHeightPaths :: Int -> Int -> [Path ()]
endHeightPaths h n
  | h == 0 = anyReturnPaths n
  | h < 0  = fmap mirror $ endHeightPaths (-h) n


-- |'splitAtReturn' 'p' splits the path 'p'
--
-- >>>
splitAtReturn :: Path a -> (Path a, Path a)
splitAtReturn = (mk *** mk) . PP.Utils.List.splitAt predicate . getUpHorizontalRenderPoints
  where
    predicate (PP.Path.Step.LStep (_, p)) = PP.Geometry.Point.getY p == 0
    predicate (PP.Path.Step.RStep (_, p)) = PP.Geometry.Point.getY p == 0

-- |'splitEveryReturn'' 'p' splits the path 'p' into minimum x-axis to x-axis paths.
--
-- >>>
splitEveryReturn :: Path a -> [Path a]
splitEveryReturn = fmap mk . PP.Utils.List.splitEvery predicate . getUpHorizontalRenderPoints
  where
    predicate = (==) 0 . PP.Geometry.Point.getY . T.snd . PP.Path.Step.getParameter

-- getPoints auxialiary function (horizontal render).
getUpHorizontalRenderPoints :: Path a -> Path (a, PP.Geometry.Point.Point)
getUpHorizontalRenderPoints = mk . L.reverse . T.snd . F.foldl f (PP.Geometry.Point.zero, []) . getSteps
    where
      f (p, acc) (PP.Path.Step.LStep _) = (p', PP.Path.Step.LStep p' : acc)
        where
          p' = PP.Geometry.Point.mv (+1) (+1) p
      f (p, acc) (PP.Path.Step.RStep _) = ((p', PP.Path.Step.LStep p'' : acc)
        where
          p'  = PP.Geometry.Point.mv (+1) (-1) p
          p'' = PP.Geometry.Point.mv (+1) 0    p

-- getPoints auxialiary function (diagonal render).
getDiagonalRenderPoints :: Path a -> Path PP.Geometry.Point.Point
getDiagonalRenderPoints = mk . L.reverse . T.snd . F.foldl f (PP.Geometry.Point.zero, []) . getSteps
    where
      f (p, acc) (PP.Path.Step.LStep _) = (p', PP.Path.Step.LStep p' : acc)
        where
          p' = PP.Geometry.Point.mv 0 (+1) p
      f (p, acc) (PP.Path.Step.RStep x) = ((p', PP.Path.Step.LStep p' : acc)
        where
          p'  = PP.Geometry.Point.mv (+1) 0 p

-- |'getPoints' 'r' 'p' associates to the path 'p' the corresponding list of points
-- according to the rendering 'r' ('Horizontal' or 'Diagonal').
getPoints :: Render -> Path a -> Path PP.Geometry.Point.Point
getPoints Horizontal = getUpHorizontalRenderPoints
getPoints Diagonal   = getDiagonalRenderPoints

-- collect all steps at a given layer.
getStepsAtLayer :: Int -> Path PP.Geometry.Point.Point -> [Step Path PP.Geometry.Point.Point]
getStepsAtLayer y = F.foldr f [] . getSteps
  where
    f (s@(PP.Path.Step.LStep (_, p)) acc
      | y == PP.Geometry.Point.getY p = s : acc
      | otherwise = acc
    f (s@(PP.Path.Step.RStep p)) acc
      | y == PP.Geometry.Point.getY p = s : acc
      | otherwise = acc

-- Stringify all steps at a given layer.
drawLayer :: Char -> Char -> [Step Path PP.Geometry.Point.Point] -> String
drawLayer cUp cDown = aux 0
  where
    aux _ [] = "\n"
    aux x (PP.Path.Step.LStep p : ss) = L.replicate (x'-x-1) ' ' ++ [cUp]   ++ aux x' ss
      where
        x' = PP.Geometry.Point.getX p
    aux x (PP.Path.Step.RStep p : ss) = L.replicate (x'-x-1) ' ' ++ [cDown] ++ aux x' ss
      where
        x' = PP.Geometry.Point.getX p

-- |'draw' 'p' stringify a path.
--
-- >>> mapM_ (putStr . (\ (i, p) -> show i ++ "\n" ++ show p ++ "\n" ++ draw p)) $ L.zip [1..] (paths 3)
-- 1
-- ()()()
-- /\/\/\
-- 2
-- ()(())
--    /\
-- /\/  \
-- 3
-- (())()
--  /\
-- /  \/\
-- 4
-- (()())
--  /\/\
-- /    \
-- 5
-- ((()))
--   /\
--  /  \
-- /    \
draw :: Path a -> String
draw = draw' defaultHorizontalPP.Path.Step.LStepChar defaultHorizontalPP.Path.Step.RStepChar

-- |'draw'' 'p' stringify a path.
--
-- >>> mapM_ (putStr . (\ (i, p) -> show i ++ "\n" ++ show p ++ "\n" ++ draw' 'u' 'd' p)) $ L.zip [1..] (paths 3)
-- 1
-- ()()()
-- ududud
-- 2
-- ()(())
--    ud
-- udu  d
-- 3
-- (())()
--  ud
-- u  dud
-- 4
-- (()())
--  udud
-- u    d
-- 5
-- ((()))
--   ud
--  u  d
-- u    d
draw' :: Char -> Char -> Path a -> String
draw' cUp cDown p = F.concat [drawLayer cUp cDown (getStepsAtLayer y ss) | y <- [maxY,maxY-1..1]]
  where
    (maxY, ss) = locateSteps p
