{-|
Module      : Data.Algorithm.PP.Perm.Class.Simple
Description : Simple permutations
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Permutations.
-}

module Data.Algorithm.PP.Perm.Class.Simple (
  simple
 ) where

import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T

import qualified Data.Algorithm.PP.Perm       as PP.Perm
import qualified Data.Algorithm.PP.PTree as PP.PTree

{- | 'isSimple' @p@ returns true iff is the permutation @p@ is simple.
-}
simple :: PP.Perm.Perm -> Bool
simple = (==) 1 . PP.PTree.height . PP.PTree.mk