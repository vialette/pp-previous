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
    safeHead
  , safeTail
  , safeLast
  ) where

import qualified Data.List as L


{- | Safe 'head' function.

>>> safeHead []
Nothing
>>> safeHead [1..5]
Just 1
-}
safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x

{- | Safe 'tail' function.

>>> safeTail []
Nothing
>>> safeTail [1..5]
Just [2,3,4,5]
-}
safeTail :: [a] -> Maybe [a]
safeTail []       = Nothing
safeTail (_ : xs) = Just xs

{- | 'safeLast' @xs@
-}
safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (L.last xs)
