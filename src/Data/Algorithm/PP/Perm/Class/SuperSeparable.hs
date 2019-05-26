module Data.Algorithm.PP.Perm.Class.SuperSeparable
  (
    separable
  ) where

import qualified Data.Algorithm.PP.Perm           as PP.Perm
import qualified Data.Algorithm.PP.SeparatingTree as PP.SuperSeparatingTree

{- | 'separable' @p@ return true iff permutatopn @p@ is separable.

-}
superSeparable :: PP.Perm.Perm -> Bool
superSeparable p = case PP.SuperSeparatingTree.mk p of
                Nothing -> False
                Just _  -> True
