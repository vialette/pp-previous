{-|
Module      : Data.Algorithm.PP.Utils.List.Safe
Description : Safe useful functions on lists
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Convenient safe functions on lists.
-}

module Data.Algorithm.PP.Utils.List.Safe
  (
  -- * Basic functions
    head
  , tail
  , last
  ) where

import Prelude hiding (head, tail, last)
import qualified Data.List as L


{- | Safe 'head' function.

>>> head []
Nothing
>>> head [1..5]
Just 1
-}
head :: [a] -> Maybe a
head []      = Nothing
head (x : _) = Just x

{- | Safe 'tail' function.

>>> tail []
Nothing
>>> tail [1..5]
Just [2,3,4,5]
-}
tail :: [a] -> Maybe [a]
tail []       = Nothing
tail (_ : xs) = Just xs

{- | 'last' @xs@
-}
last :: [a] -> Maybe a
last [] = Nothing
last xs = Just (L.last xs)
