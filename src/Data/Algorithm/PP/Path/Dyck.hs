{-|
Module      : Data.Algorithm.PP.Path.Dyck
Description : Paths
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

-}

module Data.Algorithm.PP.Path.Dyck (

  -- * Constructing
    mk
  , empty
  , fromString

  -- * Querying
  , len
  , semiLen
  , null
  , notNull

  -- * Transforming
  , getPoints
  , rev

  -- * Generating
  , paths
  , returnPaths
  ) where

import Prelude hiding (null, notNull)
import qualified Data.Foldable   as F
import qualified Data.List       as L

import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point
import qualified Data.Algorithm.PP.Combinatorics  as PP.Combinatorics
import qualified Data.Algorithm.PP.Path           as PP.Path
import qualified Data.Algorithm.PP.Path.Step      as PP.Path.Step
import qualified Data.Algorithm.PP.Utils.Maybe    as PP.Utils.Maybe


{- | 'mk' @xs@ returns a Dyck path from a list of steps @xs@.
The function returns @Nothing@ if the path is not Dyck.

>>> mk []
Just
>>> mk [UpStep, DownStep]
Just ()
>>> mk [DownStep, UpStep]
Nothing
>>> mk [UpStep, UpStep, DownStep, DownStep, UpStep, DownStep]
Just (())()
-}
mk :: [PP.Path.Step.Step] -> Maybe PP.Path.Path
mk ss = PP.Utils.Maybe.whenMaybe (check 0 ss) (PP.Path.mk ss)
  where
    check :: Int -> [PP.Path.Step.Step] -> Bool
    check h [] = h == 0
    check h (PP.Path.Step.UpStep : ss)   = check (h+1) ss
    check h (PP.Path.Step.DownStep : ss) = h > 0 && check (h-1) ss

{- | 'empty' returns the empty path. -}
empty :: PP.Path.Path
empty = PP.Path.empty

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
fromString :: String -> Maybe PP.Path.Path
fromString = mk . fmap convert
  where
    convert '(' = PP.Path.Step.UpStep
    convert ')' = PP.Path.Step.DownStep

{- | 'getPoint'
-}
getPoints :: PP.Path.Path -> [PP.Geometry.Point.Point]
getPoints = PP.Path.getPoints

{- |'len' @p@ returns the length of the path @p@.

>>> fromString "()(())" >>= Just . len
Just 6
>>> fmap len $ paths 6
[6,6,6,6,6]
-}
len :: PP.Path.Path -> Int
len = PP.Path.len

{- | 'semiLen' @p@ returns the semi-length of the path @p@.

>>> fromString "()(())" >>= Just . semiLen
Just 3
>>> fmap semiLen $ paths 6
[3,3,3,3,3]
-}
semiLen :: PP.Path.Path -> Int
semiLen = flip div 2 . len

{- | 'null' @p@ returns @True@ if the path @p@ is the empty path.

>>> fromString "" >>= Just . null
Just True
>>> fromString "()" >>= Just . null
Just False
-}
null :: PP.Path.Path -> Bool
null = PP.Path.null

{- | 'notNull' @p@ returns @True@ if the path @p@ is not the empty path.

>>> fromString "" >>= Just . notNull
Just False
>>> fromString "()" >>= Just . notNull
Just True
-}
notNull :: PP.Path.Path -> Bool
notNull = PP.Path.notNull


{- |'rev' @p@ reverses the path @p@.

>>> paths 6
[(()()),((())),()(()),(())(),()()()]
>>> fmap rev $ paths 6
[(()()),((())),(())(),()(()),()()()]
-}
rev :: PP.Path.Path -> PP.Path.Path
rev = PP.Path.rev

{- | 'returnPaths' @k@ @n@ returns all Dyck paths of length @n@ with @k@ internal returns to the x-axis.

>>> returnPaths 0 6
[(()()),((()))]
>>> returnPaths 1 6
[()(()),(())()]
>>> returnPaths 2 6
[()()()]
>>> returnPaths 3 6
[]
-}
returnPaths :: Int -> Int -> [PP.Path.Path]
returnPaths k n = F.concatMap f $ PP.Combinatorics.evenPartitions (k+1) n
    where
      f :: [Int] -> [PP.Path.Path]
      f xs = (PP.Path.mk . mconcat) <$> sequence [[[PP.Path.Step.UpStep] `mappend` ss `mappend` [PP.Path.Step.DownStep] | ss <- aux (x-2)] | x <- xs]

      aux :: Int-> [[PP.Path.Step.Step]]
      aux 0  = [[]]
      aux n' = [[PP.Path.Step.UpStep] `mappend` ss `mappend` [PP.Path.Step.DownStep] `mappend` ss' | m <- [0..n'-2]
                                                                                                   , ss  <- aux m
                                                                                                   , ss' <- aux (n'-2-m)]

{- |'paths' @n@ returns all Dyck paths of length @n@.

>>> paths 0
[]
>>> paths 1
[]
>>> paths 6
[(()()),((())),()(()),(())(),()()()]
-}
paths :: Int -> [PP.Path.Path]
paths n =  F.concat [returnPaths k n | k <- [0..n `div` 2]]