{-|
Module      : Data.Algorithm.PP.Perm.Factor.Points
Description : Trivial bijections
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Trivial bijections
-}

module Data.Algorithm.PP.Perm.Bijection.Trivial (
    -- * Trivial bijections
    inv
  , rev
  , comp

    -- * Composing trivial bijections
  , revComp
  , compRev
  , invRev
  , compInv
  , invComp
  , revInv
  , invRevComp
  ) where

import qualified Data.Foldable as F
import qualified Data.List     as L

import qualified Data.Algorithm.PP.Geometry.Point  as PP.Geometry.Point
import qualified Data.Algorithm.PP.Perm            as PP.Perm

-- | 'inv' 'perm' returns the inverse of the permutation 'perm'.
--
-- prop> inv (inv perm) = perm
--
-- >>> inv $ mk [1,3,4,2]
-- [1,4,2,3]
inv :: PP.Perm.Perm -> PP.Perm.Perm
inv = PP.Perm.mkUnsafe . fmap PP.Geometry.Point.getY . L.sortOn PP.Geometry.Point.getX . fmap PP.Geometry.Point.symmetric . PP.Perm.getPoints

-- | 'rev' 'perm' returns the reverse of the permutation 'perm'.
--
-- prop> rev (rev perm) = perm
--
-- >>> rev $ mk [1,3,4,2]
-- [2,4,3,1]
rev :: PP.Perm.Perm -> PP.Perm.Perm
rev = PP.Perm.mkUnsafe . L.reverse . PP.Perm.getList

-- | 'comp' 'perm' returns the complement of the permutation 'perm'.
--
-- prop> comp (comp perm) = perm
--
-- >>> comp $ mk [1,3,4,2]
-- [4,2,1,3]
comp :: PP.Perm.Perm -> PP.Perm.Perm
comp p = PP.Perm.mkUnsafe $ fmap (\y -> m-y+1) ys
  where
    ys = PP.Perm.getList p
    m  = F.maximum ys

-- | 'revComp' 'perm' returns the reverse complement of the permutation 'perm'.
--
-- >>> revComp $ mk [1,3,4,2]
-- [3,1,2,4]
revComp :: PP.Perm.Perm -> PP.Perm.Perm
revComp = rev . comp

-- | 'compRev' 'perm' returns the complement reverse of the permutation 'perm'.
--
-- prop> revComp p == compRev p
compRev :: PP.Perm.Perm -> PP.Perm.Perm
compRev = revComp

-- | 'compInv' 'perm' returns the complement inverse of the permutation 'perm'.
--
-- >>> compInv $ mk [1,3,4,2]
-- [4,1,3,2]
compInv :: PP.Perm.Perm -> PP.Perm.Perm
compInv = comp . inv

-- | 'invRev' 'perm' returns the inverset inverse of the permutation 'perm'.
--
-- prop> invRev p == compRev
invRev :: PP.Perm.Perm -> PP.Perm.Perm
invRev = comp . inv

-- | 'invComp' 'perm' returns the inverse complement of the permutation 'perm'.
--
-- >>> invComp $ mk [1,3,4,2]
-- [3,2,4,1]
invComp :: PP.Perm.Perm -> PP.Perm.Perm
invComp = inv . comp

-- | 'revInv' 'perm' returns the reverse inverse of the permutation 'perm'.
--
-- prop> revInv p == invComp p
revInv :: PP.Perm.Perm -> PP.Perm.Perm
revInv = invComp

-- | 'invRevComp' 'perm' returns the inverse reverse complement of permutation 'perm'.
--
-- >>> invRevComp $ mk [1,3,4,2]
-- [2,3,1,4
invRevComp :: PP.Perm.Perm -> PP.Perm.Perm
invRevComp = inv . rev . comp
