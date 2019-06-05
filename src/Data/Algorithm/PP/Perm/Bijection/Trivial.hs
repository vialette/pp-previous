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

{- | 'inv' @p@ returns the inverse of the permutation @p@.

prop> inv (inv perm) = perm

>>> inv $ mk [1,3,4,2]
[1,4,2,3]
>>> inv $ identity 4
[1,2,3,4]
-}
inv :: PP.Perm.Perm -> PP.Perm.Perm
inv = PP.Perm.mk . fmap PP.Geometry.Point.getY . L.sortOn PP.Geometry.Point.getX . fmap PP.Geometry.Point.symmetric . PP.Perm.getPoints

{- | 'rev' @p@ returns the reverse of the permutation @p@.

prop> rev (rev perm) = perm

>>> rev $ mk [1,3,4,2]
[2,4,3,1]
>>> rev $ identity 4
[4,3,2,1]
-}
rev :: PP.Perm.Perm -> PP.Perm.Perm
rev = PP.Perm.mk . L.reverse . PP.Perm.getList

{- | 'comp' @p@ returns the complement of the permutation @p@.

prop> comp (comp perm) = perm

>>> comp $ mk [1,3,4,2]
[4,2,1,3]
>>> comp $ identity 4
[4,3,2,1]
-}
comp :: PP.Perm.Perm -> PP.Perm.Perm
comp p = PP.Perm.mk $ fmap (\y -> m-y+1) ys
  where
    ys = PP.Perm.getList p
    m  = F.maximum ys

{- | 'revComp' @p@ returns the reverse complement of the permutation @p@.

>>> revComp $ mk [1,3,4,2]
[3,1,2,4]
>>> revComp $ identity 4
[1,2,3,4]
-}
revComp :: PP.Perm.Perm -> PP.Perm.Perm
revComp = rev . comp

{- | 'compRev' @p@ returns the complement reverse of the permutation @p@.

prop> revComp p == compRev p
-}
compRev :: PP.Perm.Perm -> PP.Perm.Perm
compRev = revComp

{- | 'compInv' @p@ returns the complement inverse of the permutation @p@.

>>> compInv $ mk [1,3,4,2]
[4,1,3,2]
>>> compInv $ identity 4
[4,3,2,1]
-}
compInv :: PP.Perm.Perm -> PP.Perm.Perm
compInv = comp . inv

{- | 'invRev' @p@ returns the inverset inverse of the permutation @p@.

prop> invRev p == compRev

>>> invRev $ mk [1,3,4,2]
[4,1,3,2]
>>> invRev $ identity 4
[4,3,2,1]
-}
invRev :: PP.Perm.Perm -> PP.Perm.Perm
invRev = comp . inv

{- | 'invComp' @p@ returns the inverse complement of the permutation @p@.

>>> invComp $ mk [1,3,4,2]
[3,2,4,1]
>>> invComp $ identity 4
[4,3,2,1]
-}
invComp :: PP.Perm.Perm -> PP.Perm.Perm
invComp = inv . comp

{- | 'revInv' @p@ returns the reverse inverse of the permutation @p@.

prop> revInv p == invComp p
-}
revInv :: PP.Perm.Perm -> PP.Perm.Perm
revInv = invComp

{- | 'invRevComp' @p@ returns the inverse reverse complement of permutation @p@.

>>> invRevComp $ mk [1,3,4,2]
[2,3,1,4]
>>> invRevComp $ identity 4
[1,2,3,4]
-}
invRevComp :: PP.Perm.Perm -> PP.Perm.Perm
invRevComp = inv . rev . comp
