{-|
Module      : Data.Algorithm.PP.Perm.Organization.YOrganization
Description : YOrganization numbers
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

The /x-Organization/ of a permutation \(\pi = \pi_{1} \pi_{2} \dots \pi_{n}\) of \([n]\) is
the list \(o(\pi) = (o_{1}, o_{2}, \dots, o_{n-1})\) defined by \(o_{i} = | \pi_{i+1} - \pi_{i} |\)
for \(1 \leq i \leq n-1\).
For example the yOrganization of \(\pi = 5 2 1 3 4\) is \(o(\pi) = (3, 1, 2, 1)\).

The /x-Organization number/ of a permutation \(\pi = \pi_{1} \pi_{2} \dots \pi_{n}\) of \([n]\) is
the integer \(\sum_{i=1}^{n-1} o_{i}\),
where \(o(\pi) = (o_{1}, o_{2}, \dots, o_{n-1})\) is the yOrganization of \(\pi\).

The maximal value of the yOrganization number of any permutation of \([n]\)
for \(n = 0, 1, 2, 3, \dots\) is given by \(0, 1, 3, 7, 11, 17, 23, \dots\)
(sequence [A047838](https://oeis.org/A047838)).
-}

module Data.Algorithm.PP.Perm.Organization.YOrganization (
  -- * List
    yOrganization
  , yOrganizations
  , yOrganizationAssoc
  , yOrganizationByYOrganizationNumber
  , yOrganizationFreq

  -- * Number
  , yOrganizationNumber
  , yOrganizationNumbers
  , yOrganizationNumberAssoc
  , yOrganizationNumberFreq
  ) where

import Control.Arrow
import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T

import qualified Data.Algorithm.PP.Geometry.Point           as PP.Geometry.Point
import qualified Data.Algorithm.PP.Utils.Foldable           as PP.Utils.Foldable
import qualified Data.Algorithm.PP.Utils.List               as PP.Utils.List
import qualified Data.Algorithm.PP.Perm                     as PP.Perm
import qualified Data.Algorithm.PP.Perm.Generator.Basic     as PP.Perm.Generator.Basic
import qualified Data.Algorithm.PP.Perm.Organization.Common as PP.Perm.Organization.Common

{- | 'yOrganization' @p@ returns the yOrganization of permutation @p@.

>>> mapM_ print . fmap (id &&& yOrganization) $ perms 4
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
yOrganization :: PP.Perm.Perm -> [Int]
yOrganization = F.foldr f 0 . PP.Utils.List.chunk2 . fmap PP.Geometry.Point.getX .  PP.Geometry.Point.sortOnY . PP.Perm.getPoints
  where
    f (x1, x2) acc = acc + abs (x1 - x2)

{- | 'yOrganizations' @n@ returns the yOrganizations of all permutations of length @n@.
Duplicates yOrganizations are not removed.

>>> yOrganizations 4
[[1,1,1],[1,2,1],[1,1,3],[1,2,3],[2,1,2],[2,1,2],[1,1,1],[1,2,1],[1,2,3],[2,1,2],[2,1,2],[1,1,3],[3,1,1],[3,2,1],[1,2,1],[2,1,2],[2,3,2],[1,3,1],[3,2,1],[3,1,1],[2,1,2],[1,2,1],[1,3,1],[2,3,2]]
-}
yOrganizations :: Int -> [[Int]]
yOrganizations = PP.Perm.Organization.Common.organizations yOrganization

{- | 'yOrganizationAssoc' @ps@ returns the list of pairs associating yOrganizations to
permutations in @xs@.

>>> mapM_ print . yOrganizationAssoc $ perms 4
([1,1,1],[[1,2,3,4],[4,3,2,1]])
([1,1,3],[[2,3,4,1],[3,2,1,4]])
([1,2,1],[[1,2,4,3],[2,1,3,4],[3,4,2,1],[4,3,1,2]])
([1,2,3],[[2,3,1,4],[3,2,4,1]])
([1,3,1],[[2,1,4,3],[3,4,1,2]])
([2,1,2],[[1,3,2,4],[1,3,4,2],[2,4,3,1],[3,1,2,4],[4,2,1,3],[4,2,3,1]])
([2,3,2],[[2,4,1,3],[3,1,4,2]])
([3,1,1],[[1,4,3,2],[4,1,2,3]])
([3,2,1],[[1,4,2,3],[4,1,3,2]])
-}
yOrganizationAssoc :: [PP.Perm.Perm] -> [([Int], [PP.Perm.Perm])]
yOrganizationAssoc = PP.Perm.Organization.Common.organizationAssoc yOrganization

{- | 'yOrganizationFreq' @n@ returns the list of pairs associating yOrganizations to
the number of permutations.

>>> mapM_ print . yOrganizationFreq $ perms 4
([1,1,1],2)
([1,1,3],2)
([1,2,1],4)
([1,2,3],2)
([1,3,1],2)
([2,1,2],6)
([2,3,2],2)
([3,1,1],2)
([3,2,1],2)
-}
yOrganizationFreq :: [PP.Perm.Perm] -> [([Int], Int)]
yOrganizationFreq = PP.Perm.Organization.Common.organizationAssoc yOrganization

{- | 'yOrganizationNumber' @p@ returns the yOrganization number of the permutation @p@.
The yOrganization number of a permutation is the sum of the integers of the corresponding yOrganization.

>>> mapM_ print . fmap (id &&& yOrganizationNumber) $ perms 4
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
yOrganizationNumber :: PP.Perm.Perm -> Int
yOrganizationNumber = PP.Perm.Organization.Common.organizationAssoc yOrganization

{- | 'yOrganizationNumbers' @n@ returns the yOrganization numbers of all permutations of length @n@.
Duplicates yOrganizations are not removed.

>>> yOrganizationNumbers 3
>>> yOrganizationNumbers 4
[[1,1,1],[1,2,1],[1,1,3],[1,2,3],[2,1,2],[2,1,2],[1,1,1],[1,2,1],[1,2,3],[2,1,2],[2,1,2],[1,1,3],[3,1,1],[3,2,1],[1,2,1],[2,1,2],[2,3,2],[1,3,1],[3,2,1],[3,1,1],[2,1,2],[1,2,1],[1,3,1],[2,3,2]]
-}
yOrganizationNumbers :: Int -> [[Int]]
yOrganizationNumbers = PP.Perm.Organization.Common.organizationNumbers yOrganization

{- | 'yOrganizationNumberAssoc'


-}
yOrganizationNumberAssoc :: [PP.Perm.Perm] -> [(Int, [PP.Perm.Perm])]
yOrganizationNumberAssoc = PP.Perm.Organization.Common.organizationNumberAssoc yOrganization

{- | 'yOrganizationFreq' @n@ returns the list of pairs associating the yOrganization numbers to the number of
permutation having this yOrganization number.

>>> mapM_ print $ yOrganizationNumberFreq 4
(3,2)
(4,4)
(5,12)
(6,4)
(7,2)
-}
yOrganizationNumberFreq :: [PP.Perm.Perm] -> [(Int, Int)]
yOrganizationNumberFreq = PP.Perm.Organization.Common.organizationNumberFreq yOrganization

{- | 'yOrganizationByYOrganizationNumber' @n@ returns  the list of pairs associating yOrganization numbers to
the yOrganizations.

>>> mapM_ print $ yOrganizationByYOrganizationNumber 4
(3,[[1,1,1],[1,1,1]])
(4,[[1,2,1],[1,2,1],[1,2,1],[1,2,1]])
(5,[[1,1,3],[1,1,3],[1,3,1],[1,3,1],[2,1,2],[2,1,2],[2,1,2],[2,1,2],[2,1,2],[2,1,2],[3,1,1],[3,1,1]])
(6,[[1,2,3],[1,2,3],[3,2,1],[3,2,1]])
(7,[[2,3,2],[2,3,2]])
-}
yOrganizationByYOrganizationNumber :: Int -> [(Int, [[Int]])]
yOrganizationByYOrganizationNumber =  PP.Perm.Organization.Common.organizationByYOrganizationNumber yOrganization