{-|
Module      : Data.Algorithm.PP.Path
Description : Paths
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

-}

module Data.Algorithm.PP.Path (
  -- * Type
    Path(..)

  -- * Constructing
  , mk
  , empty
  , fromString
  , upSteps
  , downSteps
  , upStepDownStep
  , upStepDownSteps
  , downStepUpStep
  , downStepUpSteps
  , upPeak
  , downPeak

  -- * Deonstructing
  , splitY

  -- * Querying
  , len
  , null
  , notNull

  -- * Transforming
  , getPoints
  , rev
  , complement

  -- *Generating
  , paths
  ) where

import Prelude hiding (null, notNull)
import Control.Arrow
import qualified Data.Foldable   as F
import qualified Data.List       as L
import qualified Data.Tuple      as T

import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point
import qualified Data.Algorithm.PP.Path.Step      as PP.Path.Step
import qualified Data.Algorithm.PP.Utils.List     as PP.Utils.List

{- Type definition -}
newtype Path = Path { getSteps :: [PP.Path.Step.Step] } deriving (Eq, Ord, Read)

--
instance Semigroup Path where
  p <> p' = mk (getSteps p ++ getSteps p')

--
instance Monoid Path where
    mempty  = mk []
    mappend = (<>)

--
instance Show Path where
  show = F.concatMap show . getSteps


{- | 'mk' @xs@ returns a path from a list of steps @xs@.

>>> mk [UpStep, UpStep, DownStep, DownStep, UpStep, DownStep]
(())()
>>> mk [DownStep, UpStep, DownStep, DownStep, UpStep, UpStep]
)())((
-}
mk :: [PP.Path.Step.Step] -> Path
mk ss = Path { getSteps = ss }

{- | 'empty' returns the empty path. -}
empty :: Path
empty = Path { getSteps = [] }

{- | 'fromString' @xs@ return a path from the well-formed paranthesis string @xs@.
The function returns @Nothing@ is @s@ is not a  well-formed paranthesis string.

>>> fromString "()"
Just ()
>>> fromString "()(())"
Just ()(())
>>> fromString "()(()))"
Nothing
>>> fromString "))(())"
Nothing
-}
fromString :: String -> Path
fromString = mk . fmap convert
  where
    convert '(' = PP.Path.Step.UpStep
    convert ')' = PP.Path.Step.DownStep

{- | 'getPoint'
-}
getPoints :: Path -> [PP.Geometry.Point.Point]
getPoints = L.reverse . T.snd . F.foldl f (PP.Geometry.Point.mkZero, []) . getSteps
    where
      f (p, acc) PP.Path.Step.UpStep = (p', p' : acc)
        where
          p' = PP.Geometry.Point.move 1 1 p
      f (p, acc) PP.Path.Step.DownStep = (p', p'' : acc)
        where
          p'  = PP.Geometry.Point.move 1 (-1) p
          p'' = PP.Geometry.Point.move 1 0    p

{- |'len' @p@ returns the length of the path @p@.

>>> fromString "()(())" >>= Just . len
Just 6
>>> fmap len $ paths 6
[6,6,6,6,6]
-}
len :: Path -> Int
len = L.length . getSteps

{- | 'null' @p@ returns @True@ if the path @p@ is the empty path.

>>> fromString "" >>= Just . null
Just True
>>> fromString "()" >>= Just . null
Just False
-}
null :: Path -> Bool
null Path { getSteps = [] } = True
null _                      = False

{- | 'notNull' @p@ returns @True@ if the path @p@ is not the empty path.

>>> fromString "" >>= Just . notNull
Just False
>>> fromString "()" >>= Just . notNull
Just True
-}
notNull :: Path -> Bool
notNull = not .null

{- | 'upSteps' @n@ returns the ascending path of length @n@. -}
upSteps :: Int -> Path
upSteps = mk . flip L.replicate PP.Path.Step.UpStep

{- | 'downSteps' @n@ returns the descending path of length @n@. -}
downSteps :: Int -> Path
downSteps = mk . flip L.replicate PP.Path.Step.DownStep

upStepDownStep :: Path
upStepDownStep = mk [PP.Path.Step.UpStep, PP.Path.Step.DownStep]

upStepDownSteps :: Int -> Path
upStepDownSteps =  mk . L.concat . flip L.replicate [PP.Path.Step.UpStep, PP.Path.Step.DownStep]

downStepUpStep :: Path
downStepUpStep = mk [PP.Path.Step.DownStep, PP.Path.Step.UpStep]

downStepUpSteps :: Int -> Path
downStepUpSteps =  complement . upStepDownSteps

upPeak :: Int -> Path
upPeak n = upSteps n <> downSteps n

downPeak :: Int -> Path
downPeak = complement . upPeak

{- |'rev' @p@ reverses the path @p@.

>>> paths 4
[)))),)))(,))(),))((,)()),)()(,)((),)(((,())),())(,()(),()((,(()),(()(,(((),((((]
>>> fmap rev $ paths 4
[((((,)(((,()((,))((,(()(,)()(,())(,)))(,(((),)((),()(),))(),(()),)()),())),))))]
-}
rev :: Path -> Path
rev = mk . L.reverse . fmap PP.Path.Step.flipStep . getSteps

complement :: Path -> Path
complement = mk . L.map PP.Path.Step.flipStep . getSteps

{- |'paths' @n@ returns all paths of length @n@.

>>> paths 0
[]
>>> paths 1
[),(]
>>> paths 2
[)),)(,(),((]
>>> paths 3
[))),))(,)(),)((,()),()(,((),(((]
>>> paths 4
[)))),)))(,))(),))((,)()),)()(,)((),)(((,())),())(,()(),()((,(()),(()(,(((),((((]
-}
paths :: Int -> [Path]
paths = fmap mk . aux
  where
    aux 0 = [[]]
    aux n = fmap (PP.Path.Step.DownStep :) ss ++ fmap (PP.Path.Step.UpStep :) ss
      where
        ss = aux (n-1)

{- | 'splitAtReturn' @p@ takes a path @p@ and returns a pair of paths @'p', p'')@, where @p'@ is the prefix of @p@
until first return at zero and @p''@ is the remaining suffix.

-}
splitY :: Int -> Path -> (Path, Path)
splitY y p = ((mk . fmap T.fst) *** (mk . fmap T.fst)) . PP.Utils.List.splitAt predicate $ L.zip (getSteps p) (getPoints p)
  where
    predicate :: (PP.Path.Step.Step, PP.Geometry.Point.Point) -> Bool
    predicate = (==) y . PP.Geometry.Point.getY . T.snd