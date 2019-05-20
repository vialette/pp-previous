module Data.Algorithm.PP.Perm.Class.Separable
  (
    separable
  ) where

import qualified Data.Algorithm.PP.Perm           as PP.Perm
import qualified Data.Algorithm.PP.SeparatingTree as PP.SeparatingTree

{- | 'separable' @p@ return true iff permutatopn @p@ is separable.

-}
separable :: PP.Perm.Perm -> Bool
separable p = case PP.SeparatingTree.mk p of
                Nothing -> False
                Just _  -> True
