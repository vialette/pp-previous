{-|
Module      : Data.Algorithm.PP.Perm.Generator
Description : Generating permutations
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Generating permutations facilities.
-}

module Data.Algorithm.PP.Perm.Generator
  (
    module M
  ) where

import Data.Algorithm.PP.Perm.Generator.Alternating as M
import Data.Algorithm.PP.Perm.Generator.Avoiding    as M
import Data.Algorithm.PP.Perm.Generator.Basic       as M
import Data.Algorithm.PP.Perm.Generator.Oscillating as M
import Data.Algorithm.PP.Perm.Generator.Random      as M
