{-|
Module      : Data.Algorithm.PP.Path.Step
Description : 2D points
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental


-}

module Data.Algorithm.PP.Path.Step (
  -- * Type
    Step(..)

  -- * Transforming
  , flipStep

  -- * Querying
  , isUpStep
  , isDownStep
  ) where

-- |Type definition.
data Step = UpStep | DownStep deriving (Eq, Ord, Read)

-- |Show instance
instance Show Step where
  show UpStep   = "("
  show DownStep = ")"

{- | 'flipStep' @s@ flips the step @s@.

>>> flipStep UpStep
)
>>> flipStep DownStep
(
-}
flipStep :: Step -> Step
flipStep UpStep   = DownStep
flipStep DownStep = UpStep

{- | 'isUpStep' @s@ returns @True@ if the step @s@ is an up-step. -}
isUpStep :: Step -> Bool
isUpStep UpStep   = True
isUpStep DownStep = False

{- | 'isDownStep' @s@ returns @True@ if the step @s@ is a down-step. -}
isDownStep :: Step -> Bool
isDownStep UpStep   = True
isDownStep DownStep = False