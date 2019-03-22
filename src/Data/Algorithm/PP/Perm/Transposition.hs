{-|
Module      : Data.Algorithm.PP.Perm.Transposition
Description : Organization numbers
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

-}

module Data.Algorithm.PP.Perm.Transposition
  (

  ) where

import qualified Control.Arrow as A
import qualified Data.Foldable as F
import qualified Data.List     as L

import qualified Data.Algorithm.PP.Perm.Generator.Basic as PP.Perm.Generator.Basic
import qualified Data.Algorithm.PP.Perm                 as PP.Perm
import qualified Data.Algorithm.PP.Utils.List           as PP.Utils.List
