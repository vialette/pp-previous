{-|
Module      : Data.Algorithm.PP.Perm.Pattern.Internal.Type
Description : Permutation internal type
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Permutation internal type.
-}

module Data.Algorithm.PP.Perm.Pattern.Internal.Type (
  -- * Type
    Seq(..)
  , PhantomPerm
  , PhantomPatt
  , Perm
  , Patt
  ) where

import qualified Data.List     as L
import qualified Data.Foldable as F

import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point

data PhantomPerm
data PhantomPatt

newtype Seq t a = Seq { getElems :: [a] }

type Perm = Seq PhantomPerm PP.Geometry.Point.Point

type Patt = Seq PhantomPatt PP.Geometry.Point.Point

class Display t where
  display :: (Show a) => Seq t a -> String

instance Display PhantomPerm where
  display Seq { getElems = xs } = "Perm " ++ show xs

instance Display PhantomPatt where
  display Seq { getElems = xs } = "Patt " ++ show xs

instance (Display t, Show a) => Show (Seq t a) where
  show = display

instance Foldable (Seq t) where
  foldr f e = F.foldr f e . getList

instance Functor (Seq t) where
  fmap f = Seq . fmap f . getList

