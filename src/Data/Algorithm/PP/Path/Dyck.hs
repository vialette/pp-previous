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
  , mk
  ) where

import qualified Data.Foldable   as F
import qualified Data.List       as L

import qualified Data.Algorithm.PP.Path      as PP.Path
import qualified Data.Algorithm.PP.Path.Step as PP.Path.Step
import qualified Data.Algorithm.Utils.Maybe  as PP.Utils.Maybe

{- | 'mk' @xs@ returns a path from a list of steps @xs@.
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
mk :: [PP.Path.Step.Step] -> Maybe PP.Path.Path
mk ss = PP.Utils.Maybe.whenMaybe (check 0 ss) (PP.Path.mk ss)
  where
    check :: Int -> [PP.Path.Step.Step] -> Bool
    check h [] = h == 0
    check h (PP.Path.Step.UpStep : ss)   = check (h+1) ss
    check h (PP.Path.Step.DownStep : ss) = h > 0 && check (h-1) ss