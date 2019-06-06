{-|
Module      : Data.Algorithm.PP.Perm.Jump
Description : organization in permutations
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

-}

module Data.Algorithm.PP.Perm.Jump (
    manhattan
  , breadth
  , jump
  , minimumJump
  ) where

import qualified Data.Foldable as F
import qualified Data.List     as L

import qualified Data.Algorithm.PP.Utils.List      as PP.Utils.List
import qualified Data.Algorithm.PP.Perm            as PP.Perm

manhattan :: PP.Perm.Perm -> Int
manhattan p = F.minimum [f i j | i <- [1 .. n-1], j <- [i+1 .. n]]
  where
    n     = PP.Perm.len p
    f i j = abs (i - j) + abs (y - y')
      where
        y  = p `PP.Perm.at` i
        y' = p `PP.Perm.at` j

breadth :: PP.Perm.Perm -> Int
breadth = manhattan

jump :: Int -> PP.Perm.Perm -> Int
jump i p = abs $ y - y'
  where
    y  = p `PP.Perm.at` i
    y' = p `PP.Perm.at` (i+1)

{- | 'minimumJump @p@ returns the minimum jump ni permutation @p@.

>>> let p = mk [3,1,4,2,5] in Jump.minimumJump  p
2
-}
minimumJump :: PP.Perm.Perm -> Int
minimumJump = F.minimum . L.map (abs . uncurry (-)) . PP.Utils.List.chunk2 . PP.Perm.getList