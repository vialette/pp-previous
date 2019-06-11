{-|
Module      : Data.Algorithm.PP.Perm.Pattern.Internal
Description : Permutation internal type
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Permutation internal type.
-}

module Data.Algorithm.PP.Perm.Pattern.Internal.Type (

  ) where

import qualified Data.List     as L
import qualified Data.Foldable as F

import qualified Data.Algorithm.PP.Geometry.Point     as PP.Geometry.Point
import qualified Data.Algorithm.PP.Perm.Internal.Type as PP.Perm.Internal.Type

class Point t where
  getPoints :: Seq t PP.Geometry.Point.Point

instance Point PhantomPerm where
  getPoints = getElems

instance Point PhantomPatt where
  getPoints = getElems

class Reduce t where
  reduce :: (Ord a) => Seq t a -> Perm

instance Reduce PhantomPerm where
  reduce = L.map T.fst . L.sortBy cmpFstSnd . L.zip [1..] . L.sortBy cmpSnd . L.zip [1..] . getElems
    where
      cmpFstSnd = compare `on` (T.fst . T.snd)
      cmpSnd    = compare `on` T.snd

instance Reduce PhantomPatt where
  reduce = L.map T.fst . L.sortBy cmpFstSnd . L.zip [1..] . L.sortBy cmpSnd . L.zip [1..] . getElems
    where
      cmpFstSnd = compare `on` (T.fst . T.snd)
      cmpSnd    = compare `on` T.snd

mkPerm :: (Foldable f, Ord a) => f a -> Perm
mkPerm = reduce

mkPatt :: (Foldable f, Ord a) => f a -> Patt
mkPatt = reduce

{- | 'len' @p@ returns the length of permutation @p@.
-}
len :: Seq t a  -> Int
len = L.length . getElems