{-|
Module      : Data.Algorithm.PP.ShuffleSquare.DB
Description : Paths
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

-}

module Data.Algorithm.PP.Perm.ShuffleSquare.DB (
    assoc
  ) where

import qualified Data.IntMap.Strict as IntMap

{- | 'assoc' -}
assoc :: [(Int, Int)]
assoc = [(2, 1), (4, 2), (6, 3), (8, 7), (10, 10)]
