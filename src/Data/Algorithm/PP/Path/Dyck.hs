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
  , fromString
  ) where

import qualified Data.Foldable   as F
import qualified Data.List       as L

import qualified Data.Algorithm.PP.Path        as PP.Path
import qualified Data.Algorithm.PP.Path.Step    as PP.Path.Step
import qualified Data.Algorithm.PP.Utils.Maybe  as PP.Utils.Maybe

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