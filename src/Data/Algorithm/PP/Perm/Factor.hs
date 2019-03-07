module Data.Algorithm.PP.Perm.Factor (
    kFactors
  , factors
  ) where

import qualified Data.Algorithm.PP.Perm           as PP.Perm
import qualified Data.Algorithm.PP.Utils.Foldable as PP.Utils.Foldable
import qualified Data.Algorithm.PP.Utils.List     as PP.Utils.List

kFactors :: Int -> PP.Perm.Perm -> [PP.Perm.SubSeq]
kFactors = fmap PP.Perm.mkSubSeq . PP.Utils.List.kFactors k . PP.Perm.getList

factors :: PP.Perm.Perm -> [PP.Perm.SubSeq]
factors p = L.concat [kFactors k | k <- [1..PP.Perm.len p]]