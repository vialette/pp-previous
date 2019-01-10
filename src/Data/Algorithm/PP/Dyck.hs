module Data.Algorithm.PP.Dyck (
    -- * Types
    Path
  , Step(..)

    -- * Steps
  , flipStep
  , isUpStep
  , isDownStep

    -- * Constructing
  , mk
  , mkUnsafe
  , paths

    -- * Transforming
  , rev
  , splitAtReturn

    -- * Querying
  , len
  , semiLen
  , getSteps

    -- * Drawing
  , draw
  , draw'

  , labelLeftToRightDown
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import qualified Data.Foldable   as F
import qualified Data.List       as L
import qualified Data.List.Split as L.Split
import qualified Data.Tuple      as T
import Prelude hiding (map)

import qualified Data.Algorithm.PP.Combinatorics  as PP.Combinatorics
import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point
import qualified Data.Algorithm.PP.Utils.List     as PP.Utils.List
import qualified Data.Algorithm.PP.Utils.Maybe    as PP.Utils.Maybe

-- |Type definition.
data Step = UpStep | DownStep deriving (Eq, Ord)

-- |Show instance
instance Show Step where
  show UpStep   = "("
  show DownStep = ")"

--
newtype Path = Path { getSteps :: [Step] } deriving (Eq, Ord)

--
-- instance Semigroup Path where
--   p <> p' = mkUnsafe (getSteps p ++ getSteps p')

--
instance Monoid Path where
    mempty  = mkUnsafe []
    mappend p p' = mkUnsafe (getSteps p ++ getSteps p')

--
instance Show Path where
  show = F.concatMap show . getSteps

-- |'flipStep' 's' flips the step 's'.
flipStep :: Step -> Step
flipStep UpStep   = DownStep
flipStep DownStep = UpStep

-- |'isUpStep' 's' returns 'True'if the step 's'is an up-step.
isUpStep :: Step -> Bool
isUpStep UpStep   = True
isUpStep DownStep = False

-- |'isDownStep' 's' returns 'True'if the step 's'is a down-step.
isDownStep :: Step -> Bool
isDownStep UpStep   = True
isDownStep DownStep = False

-- |'mk' 'ss' return a path from a list of steps 'ss'.
--
-- >>>
mk :: [Step] -> Maybe Path
mk ss = PP.Utils.Maybe.whenMaybe (check 0 ss) (mkUnsafe ss)
  where
    check :: Int -> [Step] -> Bool
    check h [] = h == 0
    check h (UpStep : ss)   = check (h+1) ss
    check h (DownStep : ss) = h > 0 && check (h-1) ss

--
mkUnsafe :: [Step] -> Path
mkUnsafe ss = Path { getSteps = ss }

-- |'len' 'path' return the length of the path 'path'.
len :: Path -> Int
len = L.length . getSteps

-- |'semiLen' 'path' return the semi-length of the path 'path'.
semiLen :: Path -> Int
semiLen = flip div 2 . len

-- Construct a path made of 'k' up-steps.
upSteps :: Int -> Path
upSteps = mkUnsafe . flip L.replicate UpStep

-- Construct a path made of 'k' down-steps.
downSteps :: Int -> Path
downSteps = mkUnsafe . flip L.replicate DownStep

-- |'rev' 'p' reverses the path 'p'.
--
-- >>>
rev :: Path -> Path
rev = mkUnsafe . L.reverse . fmap flipStep . getSteps

-- |'map' 'f' 'p'
--
-- >>>
map :: (Step -> Step) -> Path -> Maybe Path
map f = mk . fmap f . getSteps

-- |'paths' 'n' returns all Dyck paths of length 'n'.
--
-- >>>
paths :: Int -> [Path]
paths n =  F.concat [returnPosPaths k n | k <- [0..n `div`2]]

-- |'returnPosPaths' 'k' 'n' return all Dyck paths of length 'n' with 'k' internal
-- returns to the x-axis.
--
-- >>>
returnPosPaths :: Int -> Int -> [Path]
returnPosPaths k n = F.concatMap f $ PP.Combinatorics.evenPartitions (k+1) n
    where
      f :: [Int] -> [Path]
      f xs = (mkUnsafe . mconcat) <$> sequence [[[UpStep] `mappend` ss `mappend` [DownStep] | ss <- aux (x-2)] | x <- xs]

      aux :: Int-> [[Step]]
      aux 0  = [[]]
      aux n' = [[UpStep] `mappend` ss `mappend` [DownStep] `mappend` ss' | m <- [0..n'-2]
                                                                         , ss  <- aux m
                                                                         , ss' <- aux (n'-2-m)]

-- |'splitAtReturn' 'p' splits the path 'p'
--
-- >>>
splitAtReturn :: Path -> (Path, Path)
splitAtReturn p = ((mkUnsafe . fmap T.fst) *** (mkUnsafe . fmap T.fst)) . PP.Utils.List.splitAt predicate $ L.zip (getSteps p) (getRenderPoints p)
  where
    predicate :: (Step, PP.Geometry.Point.Point) -> Bool
    predicate = (==) 0 . PP.Geometry.Point.getY . T.snd

-- |'splitEveryReturn'' 'p' splits the path 'p' into minimum x-axis to x-axis paths.
--
-- >>>
splitEveryReturn :: Path -> [Path]
splitEveryReturn p = fmap (mkUnsafe . fmap T.fst) . PP.Utils.List.splitEvery predicate $ L.zip (getSteps p) (getRenderPoints p)
  where
    predicate :: (Step, PP.Geometry.Point.Point) -> Bool
    predicate = (==) 0 . PP.Geometry.Point.getY . T.snd

-- |'collapse' 'i' 'p'
--
-- >>>
collapse :: Int -> Path -> [Path]
collapse k p = fmap (mkUnsafe . fmap T.fst) . L.Split.splitWhen predicate $ L.zip (getSteps p) (getRenderPoints p)
  where
    predicate :: (Step, PP.Geometry.Point.Point) -> Bool
    predicate = (<= k) . PP.Geometry.Point.getY . T.snd

-- |'lift' 'k' 'p'
--
-- >>>
lift :: Int -> Path -> Path
lift k p = upSteps k `mappend` p `mappend` downSteps k

-- |'leftToRightMinima' 'p'
--
-- >>>
-- labelLeftToRightDown :: Path -> [Int]
labelLeftToRightDown p = T.fst . F.foldr labelUpteps ([], []) . T.fst . F.foldr labelDownSteps ([], n) $ getSteps p
  where
    n :: Int
    n = semiLen p

    -- labelDownSteps :: Step -> [(Step, Int)] -> [(Step, Int)]
    labelDownSteps UpStep   (acc, i) = ((UpStep,   0) : acc, i)
    labelDownSteps DownStep (acc, i) = ((DownStep, i) : acc, i-1)

    -- labelUpteps :: (Step, Int) -> ([Int], [(Step, Int)]) -> ([Int], [(Step, Int)])
    labelUpteps (UpStep,   _) (acc, (DownStep, i) : stack) = (i : acc, stack)
    labelUpteps (DownStep, i) (acc, stack)                 = (i : acc, (DownStep, i) : stack)
    -- labelUpteps _            _                     = error "non Dyck path"


-- Default UpStep character.
lRenderChar :: Char
lRenderChar = '/'

-- Default DownStep character.
rRenderChar :: Char
rRenderChar = '\\'

-- |'getRenderPoints' 'p'
getRenderPoints :: Path -> [PP.Geometry.Point.Point]
getRenderPoints = L.reverse . T.snd . F.foldl f (PP.Geometry.Point.zero, []) . getSteps
    where
      f (p, acc) UpStep = (p', p' : acc)
        where
          p' = PP.Geometry.Point.move 1 1 p
      f (p, acc) DownStep = (p', p'' : acc)
        where
          p'  = PP.Geometry.Point.move 1 (-1) p
          p'' = PP.Geometry.Point.move 1 0    p

-- collect all steps in a given layer.
getStepsAtLayer :: Int -> [(Step, PP.Geometry.Point.Point)] -> [(Step, PP.Geometry.Point.Point)]
getStepsAtLayer y = L.filter ((== y) . PP.Geometry.Point.getY . T.snd)

-- Stringify all steps at a given layer.
drawLayer :: (Char, Char) -> [(Step, PP.Geometry.Point.Point)] -> String
drawLayer (lChar, rChar) = aux 0
  where
    aux _ [] = "\n"
    aux x ((UpStep, p) : pss) = L.replicate (x'-x-1) ' ' ++ [lChar]   ++ aux x' pss
      where
        x' = PP.Geometry.Point.getX p
    aux x ((DownStep, p) : pss) = L.replicate (x'-x-1) ' ' ++ [rChar] ++ aux x' pss
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
draw :: Path -> String
draw = draw' (lRenderChar, rRenderChar)

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
draw' :: (Char, Char) -> Path -> String
draw' (lChar, rChar) p = F.concat [drawLayer (lChar, rChar) (getStepsAtLayer y pss) | y <- [maxY,maxY-1..1]]
  where
    ss   = getRenderPoints p
    maxY = F.maximum $ fmap PP.Geometry.Point.getY ss
    pss  = L.zip (getSteps p) ss
