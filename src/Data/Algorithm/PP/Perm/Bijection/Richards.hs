{-|
Module      : Data.Algorithm.PP.Bijection.Richards
Description :
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

-}

module Data.Algorithm.PP.Perm.Bijection.Richards (
    richards
  , invRichards
  ) where

import Control.Applicative
import qualified Data.Foldable   as F
import qualified Data.List       as L
import qualified Data.Tuple      as T

import qualified Data.Algorithm.PP.Path       as PP.Path
import qualified Data.Algorithm.PP.Path.DyckPath  as PP.Path.Dyck
import qualified Data.Algorithm.PP.Path.Step  as PP.Path.Step
import qualified Data.Algorithm.PP.Perm       as PP.Perm
import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List

{- | 'richards' @p@

-}
-- richards bijection from Dyck Path to 123-avoiding permutations.
richards :: PP.Perm.Perm -> PP.Path.Dyck.DyckPath
richards = PP.Path.mk . aux . PP.Perm.getList
  where
    aux []  = []
    aux [_] = [PP.Path.Step.UpStep, PP.Path.Step.DownStep]
    aux xs  = [PP.Path.Step.UpStep] ++ aux left ++ [PP.Path.Step.DownStep] ++ aux right
      where
        maxY = F.maximum xs
        (left, right) = PP.Utils.List.splitOn maxY xs

{- | 'invRichards' @p@

-}
invRichards :: PP.Perm.Perm -> PP.Perm.Perm
invRichards _ = PP.Perm.mk [1]
