{-|
Module      : Data.Algorithm.PP.Dyck
Description : Dyck paths
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

-}

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
  , empty
  , fromString

    -- * Transforming
  , collapse
  , lift
  , rev
  , splitAtReturn

    -- * Querying
  , len
  , Data.Algorithm.PP.Dyck.null
  , Data.Algorithm.PP.Dyck.notNull
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
data Step = UpStep | DownStep deriving (Eq, Ord, Read)

-- |Show instance
instance Show Step where
  show UpStep   = "("
  show DownStep = ")"

--
newtype Path = Path { getSteps :: [Step] } deriving (Eq, Ord, Read)

--
instance Semigroup Path where
  p <> p' = mkUnsafe (getSteps p ++ getSteps p')

--
instance Monoid Path where
    mempty  = mkUnsafe []
    mappend = (<>)

--
instance Show Path where
  show = F.concatMap show . getSteps

{- | 'flipStep' @s@ flips the step @s@.

>>> flipStep UpStep
)
>>> :t flipStep UpStep
flipStep UpStep :: Step
>>> flipStep DownStep
(
>>> :t flipStep DownStep
flipStep DownStep :: Step
-}
flipStep :: Step -> Step
flipStep UpStep   = DownStep
flipStep DownStep = UpStep

{- | 'isUpStep' @s@ returns @True@ if the step @s@ is an up-step.
-}
isUpStep :: Step -> Bool
isUpStep UpStep   = True
isUpStep DownStep = False

{- | 'isDownStep' @s@ returns @True@ if the step @s@ is a down-step.
-}
isDownStep :: Step -> Bool
isDownStep UpStep   = True
isDownStep DownStep = False

{- |'mk' @xs@ returns a path from a list of steps @xs@.
The function returns @Nothing@ if the path is not well-formed.

>>> mk []
Just
>>> mk [UpStep, DownStep]
Just ()
>>> mk [DownStep, UpStep]
Nothing
>>> mk [UpStep, UpStep, DownStep, DownStep, UpStep, DownStep]
Just (())()
-}
mk :: [Step] -> Maybe Path
mk ss = PP.Utils.Maybe.whenMaybe (check 0 ss) (mkUnsafe ss)
  where
    check :: Int -> [Step] -> Bool
    check h [] = h == 0
    check h (UpStep : ss)   = check (h+1) ss
    check h (DownStep : ss) = h > 0 && check (h-1) ss

-- Construct a path from a list of lists.
mkUnsafe :: [Step] -> Path
mkUnsafe ss = Path { getSteps = ss }

{- | 'empty' returns the empty path.
-}
empty :: Path
empty = Path { getSteps = [] }

{- | 'fromString' @xs@ return a path from the well-aprantesis string @xs@.

>>> fromString "()"
Just ()
>>> fromString "()(())"
Just ()(())
>>> fromString "()(()))"
Nothing
>>> fromString "))(())"
Nothing
-}
fromString :: String -> Maybe Path
fromString = mk . fmap f
  where
    f '(' = UpStep
    f ')' = DownStep

{- |'len' @p@ returns the length of the path @p@.

>>> fmap len $ paths 6
[6,6,6,6,6]
-}
len :: Path -> Int
len = L.length . getSteps

{- | 'semiLen' @p@ returns the semi-length of the path @p@.

>>> fmap semiLen $ paths 6
[3,3,3,3,3]
-}
semiLen :: Path -> Int
semiLen = flip div 2 . len

{- | 'null' @p@ returns @True@ if the path @p@ is the empty path.

>>> fromString "" >>= Just . null
Just True
>>> fromString "()" >>= null
Just False
-}
null :: Path -> Bool
null p = len p == 0

{- | 'null' @p@ returns @True@ if the path @p@ is not the empty path.

>>> fromString "" >>= Just . null
Just False
>>> fromString "()" >>= null
Just True
-}
notNull :: Path -> Bool
notNull = not . Data.Algorithm.PP.Dyck.null

-- Construct a path made of 'k' up-steps.
upSteps :: Int -> Path
upSteps = mkUnsafe . flip L.replicate UpStep

-- Construct a path made of 'k' down-steps.
downSteps :: Int -> Path
downSteps = mkUnsafe . flip L.replicate DownStep

{- |'rev' @p@ reverses the path @p@.

>>> paths 8
[(()()()),(()(())),((())()),((()())),(((()))),()(()()),()((())),(())(()),(()())(),((()))(),()()(()),()(())(),(())()(),()()()()]
>>> fmap rev $ paths 8
[(()()()),((())()),(()(())),((()())),(((()))),(()())(),((()))(),(())(()),()(()()),()((())),(())()(),()(())(),()()(()),()()()()]
-}
rev :: Path -> Path
rev = mkUnsafe . L.reverse . fmap flipStep . getSteps

-- |'map' 'f' 'p'
--
-- >>>
map :: (Step -> Step) -> Path -> Maybe Path
map f = mk . fmap f . getSteps

{- |'paths' @n@ returns all Dyck paths of length 'n'.

>>> paths 0
[]
>>> paths 1
[]
>>> paths 6
[(()()),((())),()(()),(())(),()()()]
paths 8
[(()()()),(()(())),((())()),((()())),(((()))),()(()()),()((())),(())(()),(()())(),((()))(),()()(()),()(())(),(())()(),()()()()]

-}
paths :: Int -> [Path]
paths n =  F.concat [returnPosPaths k n | k <- [0..n `div` 2]]

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

{- |'collapse' 'i' 'p'

>>> p = fromString "((()()()()())((()))()()(()()()))((()))"
>>> p >>= Just . filter notNull . collapse 1
Just [(()()()()())((()))()()(()()()),(())]
>>> p >>= Just . filter notNull . collapse 2
Just [()()()()(),(()),()()(),()]
>>> p >>= Just . filter notNull . collapse 3
Just [()]
>>> p >>= Just . filter notNull . collapse 4
Just []
-}
collapse :: Int -> Path -> [Path]
collapse k p = fmap (mkUnsafe . fmap T.fst) . L.Split.splitWhen predicate $ L.zip (getSteps p) (getRenderPoints p)
  where
    predicate :: (Step, PP.Geometry.Point.Point) -> Bool
    predicate = (<= k) . PP.Geometry.Point.getY . T.snd

{- |'lift' @k@ @p@

>>> paths 4
[(()),()()]
>>> fmap (lift 1) $ paths 4
[((())),(()())]
>>> fmap (lift 2) $ paths 4
[(((()))),((()()))]
>>> fmap (lift 3) $ paths 4
[((((())))),(((()())))]
-}
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

{- | 'draw' @p@ stringify the path @p@ using @/@ for an upstep and @\@ for a
downstep.

>>> let f (i, p) = intercalate "\n" [show i, show p, draw p] in mapM_ (putStr . f) . zip [1..] $ paths 6
1
(()())
 /\/\
/    \
2
((()))
  /\
 /  \
/    \
3
()(())
   /\
/\/  \
4
(())()
 /\
/  \/\
5
()()()
/\/\/\
-}
draw :: Path -> String
draw = draw' (lRenderChar, rRenderChar)

{- | 'draw' @(lChar, rChar)@ @p@ stringify the path @p@ using @lChar@ for an upstep
and @rChar@ for a downstep.

>>> let f (i, p) = intercalate "\n" [show i, show p, draw' ('u', 'd') p] in mapM_ (putStr . f) . zip [1..] $ paths 6
1
(()())
 udud
u    d
2
((()))
  ud
 u  d
u    d
3
()(())
   ud
udu  d
4
(())()
 ud
u  dud
5
()()()
ududud
-}
draw' :: (Char, Char) -> Path -> String
draw' (lChar, rChar) p = F.concat [drawLayer (lChar, rChar) (getStepsAtLayer y pss) | y <- [maxY,maxY-1..1]]
  where
    ss   = getRenderPoints p
    maxY = F.maximum $ fmap PP.Geometry.Point.getY ss
    pss  = L.zip (getSteps p) ss
