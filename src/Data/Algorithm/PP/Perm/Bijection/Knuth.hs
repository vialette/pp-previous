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

import qualified Data.Algorithm.PP.Path            as PP.Path
import qualified Data.Algorithm.PP.Path.DyckPath       as PP.Path.Dyck
import qualified Data.Algorithm.PP.Path.Step       as PP.Path.Step
import qualified Data.Algorithm.PP.Perm            as PP.Perm
import qualified Data.Algorithm.PP.Utils.List      as PP.Utils.List
import qualified Data.Algorithm.PP.StdYoungTableau as PP.StdYoungTableau

{- | 'knuth132AvoidingPermToDyckPath' @p@

  bijection from
  132-avoiding permutations to Dyck paths.

>>> knuth $ mk [7,5,6,4,2,1,3]
()(())()(()())
-}
knuth132AvoidingPermToDyckPath :: PP.Perm.Perm -> PP.Path.Path
knuth132AvoidingPermToDyckPath = PP.Path.mk . aux . PP.Perm.getList
  where
    aux []  = []
    aux [_] = [PP.Path.Step.UpStep, PP.Path.Step.DownStep]
    aux xs  = [PP.Path.Step.UpStep] ++ aux left ++ [PP.Path.Step.DownStep] ++ aux right
      where
        maxY = F.maximum xs
        (left, right) = PP.Utils.List.splitOn maxY xs

{- | 'knuthDyckPathTo132AvoidingPerm' @p@

-}
knuthDyckPathTo132AvoidingPerm :: PP.Path.Path -> PP.Perm.Perm
knuthDyckPathTo132AvoidingPerm _ = PP.Perm.mk [1 :: Int]

{- |'knuth321AvoidingPermToDyckPath' @p@
-}
knuth321AvoidingPermToDyckPath :: PP.Perm.Perm -> PP.Path.Path
knuth321AvoidingPermToDyckPath perm = PP.Path.mk (left ++ L.reverse right)
  where
    ys = PP.Perm.getList perm

    (pTableau, qTableau) = PP.StdYoungTableau.robinsonSchensted perm

    firstRowPTableau = L.head $ PP.StdYoungTableau.getRows pTableau
    left = F.foldr f [] ys
      where
        f y acc
          | y `L.elem` firstRowPTableau = PP.Path.Step.UpStep   : acc
          | otherwise                   = PP.Path.Step.DownStep : acc

    firstRowQTableau = L.head $ PP.StdYoungTableau.getRows qTableau
    right = F.foldr f [] ys
      where
        f y acc
          | y `L.elem` firstRowQTableau = PP.Path.Step.DownStep : acc
          | otherwise                   = PP.Path.Step.UpStep   : acc

{- | 'knuthDyckPathTo321AvoidingPerm' @p@
-}
knuthDyckPathTo321AvoidingPerm :: PP.Path.Path -> PP.Perm.Perm
knuthDyckPathTo321AvoidingPerm _ = PP.Perm.mk [1 :: Int]

{- | 'knuth132AvoidingPermTo321AvoidingPerm' @p@
-}
knuth132AvoidingPermTo321AvoidingPerm :: PP.Perm.Perm -> PP.Perm.Perm
knuth132AvoidingPermTo321AvoidingPerm = knuthDyckPathTo321AvoidingPerm . knuth132AvoidingPermToDyckPath

{- | 'knuth321AvoidingPermTo132AvoidingPerm' @p@
-}
knuth321AvoidingPermTo132AvoidingPerm :: PP.Perm.Perm -> PP.Perm.Perm
knuth321AvoidingPermTo132AvoidingPerm = knuthDyckPathTo132AvoidingPerm . knuth321AvoidingPermToDyckPath
