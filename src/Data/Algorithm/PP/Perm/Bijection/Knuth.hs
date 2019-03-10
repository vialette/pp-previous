{-|
Module      : Data.Algorithm.PP.Bijection.Knuth
Description :
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

-}

module Data.Algorithm.PP.Perm.Bijection.Knuth (
    knuth132AvoidingPermTo321AvoidingPerm
  , knuth321AvoidingPermTo132AvoidingPerm
  ) where

import qualified Data.Foldable  as F
import qualified Data.List      as L

import qualified Data.Algorithm.PP.Dyck            as PP.Dyck
import qualified Data.Algorithm.PP.Perm            as PP.Perm
import qualified Data.Algorithm.PP.Utils.List      as PP.Utils.List
import qualified Data.Algorithm.PP.StdYoungTableau as PP.StdYoungTableau

-- 'knuth132AvoidingPermToDyckPath' 'perm'
-- bijection from
-- 132-avoiding permutations to Dyck paths.
--
-- >>> knuth (mk [7,5,6,4,2,1,3])
-- ()(())()(()())
knuth132AvoidingPermToDyckPath :: PP.Perm.Perm -> PP.Dyck.Path
knuth132AvoidingPermToDyckPath = PP.Dyck.mkUnsafe . aux . PP.Perm.getList
  where
    aux []  = []
    aux [_] = [PP.Dyck.UpStep, PP.Dyck.DownStep]
    aux xs  = [PP.Dyck.UpStep] ++ aux left ++ [PP.Dyck.DownStep] ++ aux right
      where
        maxY = F.maximum xs
        (left, right) = PP.Utils.List.splitOn maxY xs

{- | 'knuthDyckPathTo132AvoidingPerm' @p@

-}
knuthDyckPathTo132AvoidingPerm :: PP.Dyck.Path -> PP.Perm.Perm
knuthDyckPathTo132AvoidingPerm _ = PP.Perm.mk [1 :: Int]

-- |'knuth321AvoidingPermToDyckPath' 'perm'
knuth321AvoidingPermToDyckPath :: PP.Perm.Perm -> PP.Dyck.Path
knuth321AvoidingPermToDyckPath perm = PP.Dyck.mkUnsafe (left ++ L.reverse right)
  where
    ys = PP.Perm.getList perm

    (pTableau, qTableau) = PP.StdYoungTableau.robinsonSchensted perm

    firstRowPTableau = L.head $ PP.StdYoungTableau.getRows pTableau
    left = F.foldr f [] ys
      where
        f y acc
          | y `L.elem` firstRowPTableau = PP.Dyck.UpStep   : acc
          | otherwise                   = PP.Dyck.DownStep : acc

    firstRowQTableau = L.head $ PP.StdYoungTableau.getRows qTableau
    right = F.foldr f [] ys
      where
        f y acc
          | y `L.elem` firstRowQTableau = PP.Dyck.DownStep : acc
          | otherwise                   = PP.Dyck.UpStep   : acc

-- |'knuthDyckPathTo321AvoidingPerm' 'path'
knuthDyckPathTo321AvoidingPerm :: PP.Dyck.Path -> PP.Perm.Perm
knuthDyckPathTo321AvoidingPerm _ = PP.Perm.mk [1 :: Int]

-- |'knuth132AvoidingPermTo321AvoidingPerm' 'perm'
knuth132AvoidingPermTo321AvoidingPerm :: PP.Perm.Perm -> PP.Perm.Perm
knuth132AvoidingPermTo321AvoidingPerm = knuthDyckPathTo321AvoidingPerm . knuth132AvoidingPermToDyckPath

-- |'knuth321AvoidingPermTo132AvoidingPerm' 'perm'
knuth321AvoidingPermTo132AvoidingPerm :: PP.Perm.Perm -> PP.Perm.Perm
knuth321AvoidingPermTo132AvoidingPerm = knuthDyckPathTo132AvoidingPerm . knuth321AvoidingPermToDyckPath
