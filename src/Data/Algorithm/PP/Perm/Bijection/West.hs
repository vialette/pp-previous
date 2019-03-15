{-|
Module      : Data.Algorithm.PP.Perm.Factor.Points
Description : West bijections
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

West bijections
-}

module Data.Algorithm.PP.Perm.Bijection.West (
    west
  , invWest
  ) where

import qualified Data.Algorithm.PP.Perm as PP.Perm

{- | 'west' @p@ returns
-}
west :: PP.Perm.Perm -> PP.Perm.Perm
west _ = PP.Perm.mk [1]

{- | 'invWest' @p@ returns
-}
invWest :: PP.Perm.Perm -> PP.Perm.Perm
invWest _ = PP.Perm.mk [1]
