{-|
Module      : Data.Algorithm.PP.Bijection.KnuthRichards
Description :
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

-}

module Data.Algorithm.PP.Perm.Bijection.KnuthRichards (
    knuthRichards
  , invKnuthRichards
  ) where

import Control.Applicative
import qualified Data.Foldable   as F
import qualified Data.List       as L
import qualified Data.Tuple      as T

import qualified Data.Algorithm.PP.Dyck            as PP.Dyck
import qualified Data.Algorithm.PP.Perm            as PP.Perm
import qualified Data.Algorithm.PP.Utils.List      as PP.Utils.List


{- | 'knuthRichards' @p@

-}
knuthRichards :: PP.Perm.Perm -> PP.Perm.Perm
knuthRichards _ = PP.Perm.mk [1] -- PP.Perm.Bijection.Richards.richards . PP.Perm.Bijection.Knuth.knuth

{- | 'invKnuthRichards' @p@

-}
invKnuthRichards :: PP.Perm.Perm -> PP.Perm.Perm
invKnuthRichards _ = PP.Perm.mk [1]
