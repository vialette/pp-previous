{-|
Module      : Data.Algorithm.PP.Perm.Organization
Description : organization in permutations
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

-}

module Data.Algorithm.PP.Perm.Organization (
    xOrganization
  , xOrganizationNumber
  , xPalindromic

    yOrganization
  , yOrganizationNumber
  , yPalindromic

  , palindromic
  ) where

import qualified Data.Foldable as F
import qualified Data.List     as L

import qualified Data.Algorithm.PP.Utils.Foldable           as PP.Utils.Foldable
import qualified Data.Algorithm.PP.Utils.List               as PP.Utils.List
import qualified Data.Algorithm.PP.Perm                     as PP.Perm

{- | 'xOrganization' @p@ returns the xOrganization of permutation @p@.

>>> mapM_ print . fmap (id &&& xOrganization) $ perms 3
([1,2,3],[1,1])
([2,1,3],[1,2])
([3,2,1],[1,1])
([2,3,1],[1,2])
([3,1,2],[2,1])
([1,3,2],[2,1])
-}
xOrganization :: PP.Perm.Perm -> [Int]
xOrganization = L.map (abs . uncurry (-)) . PP.Utils.List.chunk2 . PP.Perm.getList

{- | 'xOrganizationNumber' @p@ returns the xOrganization number of the permutation @p@.
The xOrganization number of a permutation is the sum of the integers of the corresponding xOrganization.

>>> mapM_ print . fmap (id &&& xOrganizationNumber) $ perms 4
([1,2,3,4],3)
([2,1,3,4],4)
([3,2,1,4],5)
([2,3,1,4],6)
([3,1,2,4],5)
([1,3,2,4],5)
([4,3,2,1],3)
([3,4,2,1],4)
([3,2,4,1],6)
([4,2,3,1],5)
([2,4,3,1],5)
([2,3,4,1],5)
([4,1,2,3],5)
([1,4,2,3],6)
([1,2,4,3],4)
([4,2,1,3],5)
([2,4,1,3],7)
([2,1,4,3],5)
([4,1,3,2],6)
([1,4,3,2],5)
([1,3,4,2],5)
([4,3,1,2],4)
([3,4,1,2],5)
([3,1,4,2],7)
-}
xOrganizationNumber :: PP.Perm.Perm -> Int
xOrganizationNumber = F.sum . xOrganization

xPalindromic :: PP.Perm.Perm -> Bool
xPalindromic p = xo == L.reverse xo
  where
    xo = xOrganization p

{- | 'yOrganization' @p@ returns the yOrganization of permutation @p@.

>>> mapM_ print . fmap (id &&& yOrganization) $ perms 3
([1,2,3],[1,1])
([2,1,3],[1,2])
([3,2,1],[1,1])
([2,3,1],[2,1])
([3,1,2],[1,2])
([1,3,2],[2,1])
-}
yOrganization :: PP.Perm.Perm -> [Int]
yOrganization = L.map f . PP.Utils.List.chunk2 . L.map PP.Geometry.Point.getX . PP.Geometry.Point.sortOnY . PP.Perm.getPoints
  where
    f (x1, x2) = abs (x1 - x2)

{- | 'yOrganizationNumber' @p@ returns the yOrganization number of the permutation @p@.
The yOrganization number of a permutation is the sum of the integers of the corresponding yOrganization.

>>> mapM_ print . fmap (id &&& yOrganizationNumber) $ perms 4
([1,2,3,4],3)
([2,1,3,4],4)
([3,2,1,4],5)
([2,3,1,4],5)
([3,1,2,4],6)
([1,3,2,4],5)
([4,3,2,1],3)
([3,4,2,1],4)
([3,2,4,1],5)
([4,2,3,1],5)
([2,4,3,1],6)
([2,3,4,1],5)
([4,1,2,3],5)
([1,4,2,3],5)
([1,2,4,3],4)
([4,2,1,3],6)
([2,4,1,3],7)
([2,1,4,3],5)
([4,1,3,2],5)
([1,4,3,2],5)
([1,3,4,2],6)
([4,3,1,2],4)
([3,4,1,2],5)
([3,1,4,2],7)
-}
yOrganizationNumber :: PP.Perm.Perm -> Int
yOrganizationNumber = F.sum . yOrganization

yPalindromic :: PP.Perm.Perm -> Bool
yPalindromic p = yo == L.reverse yo
  where
    yo = yOrganization p

palindromic :: PP.Perm.Perm -> Bool
palindromic p = xPalindromic p && yPalindromic p
