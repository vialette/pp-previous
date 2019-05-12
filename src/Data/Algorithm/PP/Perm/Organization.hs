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
  , yOrganization
  , stronglyEquallyOrganized

  , xOrganizationNumber
  , yOrganizationNumber
  , organizationNumber

  , xPalindromic
  , yPalindromic
  , palindromic
  ) where

import qualified Data.Foldable as F
import qualified Data.List     as L

import qualified Data.Algorithm.PP.Geometry.Point  as PP.Geometry.Point
import qualified Data.Algorithm.PP.Utils.Foldable  as PP.Utils.Foldable
import qualified Data.Algorithm.PP.Utils.List      as PP.Utils.List
import qualified Data.Algorithm.PP.Perm            as PP.Perm

{- | 'xOrganization' @p@ returns the xOrganization of permutation @p@.

>>> mapM_ print . fmap (id &&& xOrganization) $ perms 4
([1,2,3,4],[1,1,1])
([2,1,3,4],[1,2,1])
([3,2,1,4],[1,1,3])
([2,3,1,4],[1,2,3])
([3,1,2,4],[2,1,2])
([1,3,2,4],[2,1,2])
([4,3,2,1],[1,1,1])
([3,4,2,1],[1,2,1])
([3,2,4,1],[1,2,3])
([4,2,3,1],[2,1,2])
([2,4,3,1],[2,1,2])
([2,3,4,1],[1,1,3])
([4,1,2,3],[3,1,1])
([1,4,2,3],[3,2,1])
([1,2,4,3],[1,2,1])
([4,2,1,3],[2,1,2])
([2,4,1,3],[2,3,2])
([2,1,4,3],[1,3,1])
([4,1,3,2],[3,2,1])
([1,4,3,2],[3,1,1])
([1,3,4,2],[2,1,2])
([4,3,1,2],[1,2,1])
([3,4,1,2],[1,3,1])
([3,1,4,2],[2,3,2])
-}
xOrganization :: PP.Perm.Perm -> [Int]
xOrganization = L.map (abs . uncurry (-)) . PP.Utils.List.chunk2 . PP.Perm.getList

{- | 'yOrganization' @p@ returns the yOrganization of permutation @p@.

>>> mapM_ print . fmap (id &&& yOrganization) $ perms 4
([1,2,3,4],[1,1,1])
([2,1,3,4],[1,2,1])
([3,2,1,4],[1,1,3])
([2,3,1,4],[2,1,2])
([3,1,2,4],[1,2,3])
([1,3,2,4],[2,1,2])
([4,3,2,1],[1,1,1])
([3,4,2,1],[1,2,1])
([3,2,4,1],[2,1,2])
([4,2,3,1],[2,1,2])
([2,4,3,1],[3,2,1])
([2,3,4,1],[3,1,1])
([4,1,2,3],[1,1,3])
([1,4,2,3],[2,1,2])
([1,2,4,3],[1,2,1])
([4,2,1,3],[1,2,3])
([2,4,1,3],[2,3,2])
([2,1,4,3],[1,3,1])
([4,1,3,2],[2,1,2])
([1,4,3,2],[3,1,1])
([1,3,4,2],[3,2,1])
([4,3,1,2],[1,2,1])
([3,4,1,2],[1,3,1])
([3,1,4,2],[2,3,2])
-}
yOrganization :: PP.Perm.Perm -> [Int]
yOrganization = L.map f . PP.Utils.List.chunk2 . L.map PP.Geometry.Point.getX . PP.Geometry.Point.sortOnY . PP.Perm.getPoints
  where
    f (x1, x2) = abs (x1 - x2)

{- | 'stronglyEquallyOrganized' @p@  returns true iff the permutations @p@ has identical
x-organization and y-organization.

>>> stronglyEquallyOrganized $ mk [5,6,4,3,1,2]
True
>>> let p = mk [5,6,4,3,1,2] in (xOrganization p, yOrganization p)
([1,2,1,2,1],[1,2,1,2,1])
-}
stronglyEquallyOrganized :: PP.Perm.Perm -> Bool
stronglyEquallyOrganized p = xo == yo
  where
    xo = xOrganization p
    yo = yOrganization p

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
>>> let f = (minimum &&& maximum) . map xOrganizationNumber . perms in [f i | i <- [2..10]]
[(1,1),(2,3),(3,7),(4,11),(5,17),(6,23),(7,31),(8,39),(9,49)]
-}
xOrganizationNumber :: PP.Perm.Perm -> Int
xOrganizationNumber = F.sum . xOrganization

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
>>> let f = (minimum &&& maximum) . map yOrganizationNumber . perms in [f i | i <- [2..10]]
[(1,1),(2,3),(3,7),(4,11),(5,17),(6,23),(7,31),(8,39),(9,49)]
-}
yOrganizationNumber :: PP.Perm.Perm -> Int
yOrganizationNumber = F.sum . yOrganization

{- | 'organizationNumber' @p@ returns the difference between the x-organization number
and the y-organization number of the permutation @p@.

>>> let p = mk [5,1,6,4,3,2] in organizationNumber p
2
>>> let p = mk [5,1,6,4,3,2]  in (xOrganizationNumber p, yOrganizationNumber p)
(13,11)
>>> let p = mk [2,1,5,6,4,3] in organizationNumber p
-1
>>> let p = mk [2,1,5,6,4,3] in (xOrganizationNumber p, yOrganizationNumber p)
(9,10)
>>> let p = mk [5,6,4,3,1,2] in organizationNumber p
0
>>> let p = mk [5,6,4,3,1,2] in (xOrganizationNumber p, yOrganizationNumber p)
(7,7)
-}
organizationNumber :: PP.Perm.Perm -> Int
organizationNumber p = xon - yon
  where
    xon = xOrganizationNumber p
    yon = yOrganizationNumber p

{- | 'xPalindromic' @p@  returns true iff the x-organization of the permutation @p@ is
palindromic.

>>> xPalindromic $ mk [4,1,2,5,6,3]
True
>>> xOrganization $ mk [4,1,2,5,6,3]
[3,1,3,1,3]
>>> xPalindromic $ mk [4,1,6,2,5,3]
False
>>> xOrganization $ mk [4,1,6,2,5,3]
[3,5,4,3,2]
>>> let f = length . filter yPalindromic . perms in [f i | i <- [2..10]]
[2,2,16,8,88,60,960,424,6656]
-}
xPalindromic :: PP.Perm.Perm -> Bool
xPalindromic p = xo == L.reverse xo
  where
    xo = xOrganization p

{- | 'yPalindromic' @p@  returns true iff the y-organization of the permutation @p@ is
palindromic.

>>> let p = mk [4,1,2,5,6,3] in yPalindromic p
True
>>> let p = mk [4,1,2,5,6,3] in yOrganization p
[1,3,5,3,1]
>>> let p = mk [4,1,6,2,5,3] in yPalindromic p
False
>>> let p = mk [4,1,6,2,5,3] in yOrganization p
[2,2,5,4,2]
>>> let f = length . filter yPalindromic . perms in [f i | i <- [2..10]]
[2,2,16,8,88,60,960,424,6656]
-}
yPalindromic :: PP.Perm.Perm -> Bool
yPalindromic p = yo == L.reverse yo
  where
    yo = yOrganization p

{- | 'palindromic' @p@ returns true iff both the x-organization and the y-organization
of the permutation @p@ are palindromic.

>>> let p = mk [4,5,6,1,2,3] in palindromic p
True
>>> let p = mk [4,5,6,1,2,3] in (xOrganization p, yOrganization p)
([1,1,5,1,1],[1,1,5,1,1])
>>> let f = length . filter palindromic . perms in [f i | i <- [2..10]]
[2,2,12,8,56,48,444,384,3856]
-}
palindromic :: PP.Perm.Perm -> Bool
palindromic p = xPalindromic p && yPalindromic p
