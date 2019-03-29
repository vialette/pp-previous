{-|
Module      : Data.Algorithm.PP.Perm.Organization.XOrganization
Description : XOrganization numbers
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

The /x-Organization/ of a permutation \(\pi = \pi_{1} \pi_{2} \dots \pi_{n}\) of \([n]\) is
the list \(o(\pi) = (o_{1}, o_{2}, \dots, o_{n-1})\) defined by \(o_{i} = | \pi_{i+1} - \pi_{i} |\)
for \(1 \leq i \leq n-1\).
For example the xOrganization of \(\pi = 5 2 1 3 4\) is \(o(\pi) = (3, 1, 2, 1)\).

The /x-Organization number/ of a permutation \(\pi = \pi_{1} \pi_{2} \dots \pi_{n}\) of \([n]\) is
the integer \(\sum_{i=1}^{n-1} o_{i}\),
where \(o(\pi) = (o_{1}, o_{2}, \dots, o_{n-1})\) is the xOrganization of \(\pi\).

The maximal value of the xOrganization number of any permutation of \([n]\)
for \(n = 0, 1, 2, 3, \dots\) is given by \(0, 1, 3, 7, 11, 17, 23, \dots\)
(sequence [A047838](https://oeis.org/A047838)).
-}

module Data.Algorithm.PP.Perm.Organization.XOrganization (
  -- * List
    xOrganization
  , xOrganizations
  , xOrganizationAssoc
  , xOrganizationByXOrganizationNumber
  , xOrganizationFreq

  -- * Number
  , xOrganizationNumber
  , xOrganizationNumbers
  , xOrganizationNumberAssoc
  , xOrganizationNumberFreq
  ) where

import Control.Arrow
import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T

import qualified Data.Algorithm.PP.Utils.Foldable           as PP.Utils.Foldable
import qualified Data.Algorithm.PP.Utils.List               as PP.Utils.List
import qualified Data.Algorithm.PP.Perm                     as PP.Perm
import qualified Data.Algorithm.PP.Perm.Generator.Basic     as PP.Perm.Generator.Basic
import qualified Data.Algorithm.PP.Perm.Organization.Common as PP.Perm.Organization.Common


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

{- | 'xOrganizations' @n@ returns the xOrganizations of all permutations of length @n@.
Duplicates xOrganizations are not removed.

>>> xOrganizations 3
[[1,1],[1,2],[1,1],[1,2],[2,1],[2,1]]
-}
xOrganizations :: Int -> [[Int]]
xOrganizations = PP.Perm.Organization.Common.organizations xOrganization

{- | 'xOrganizationAssoc' @ps@ returns the list of pairs associating xOrganizations to
permutations in @xs@.

>>> mapM_ print . xOrganizationAssoc $ perms 4
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
xOrganizationAssoc :: [PP.Perm.Perm] -> [([Int], [PP.Perm.Perm])]
xOrganizationAssoc = PP.Perm.Organization.Common.organizationAssoc xOrganization

{- | 'xOrganizationFreq' @n@ returns the list of pairs associating xOrganizations to
the number of permutations.

>>> mapM_ print . xOrganizationFreq $ perms 4
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
xOrganizationFreq :: [PP.Perm.Perm] -> [([Int], Int)]
xOrganizationFreq = PP.Perm.Organization.Common.organizationFreq xOrganization

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
xOrganizationNumber = PP.Perm.Organization.Common.organizationNumber xOrganization

{- | 'xOrganizationNumbers' @n@ returns the xOrganization numbers of all permutations of length @n@.
Duplicates xOrganizations are not removed.

>>> xOrganizationNumbers 3
[[1,1],[1,2],[1,1],[1,2],[2,1],[2,1]]
-}
xOrganizationNumbers :: Int -> [[Int]]
xOrganizationNumbers = PP.Perm.Organization.Common.organizationNumbers xOrganization

{- | 'xOrganizationNumberAssoc'

>>> mapM_ print $ xOrganizationNumberAssoc $ perms 4
(3,[[1,2,3,4],[4,3,2,1]])
(4,[[1,2,4,3],[2,1,3,4],[3,4,2,1],[4,3,1,2]])
(5,[[1,3,2,4],[1,3,4,2],[1,4,3,2],[2,1,4,3],[2,3,4,1],[2,4,3,1],[3,1,2,4],[3,2,1,4],[3,4,1,2],[4,1,2,3],[4,2,1,3],[4,2,3,1]])
(6,[[1,4,2,3],[2,3,1,4],[3,2,4,1],[4,1,3,2]])
(7,[[2,4,1,3],[3,1,4,2]])
-}
xOrganizationNumberAssoc :: [PP.Perm.Perm] -> [(Int, [PP.Perm.Perm])]
xOrganizationNumberAssoc = PP.Perm.Organization.Common.organizationNumberAssoc xOrganization

{- | 'xOrganizationFreq' @n@ returns the list of pairs associating the xOrganization numbers to the number of
permutation having this xOrganization number.

>>> mapM_ print $ xOrganizationNumberFreq $ perms 4
(3,2)
(4,4)
(5,12)
(6,4)
(7,2)
-}
xOrganizationNumberFreq :: [PP.Perm.Perm] -> [(Int, Int)]
xOrganizationNumberFreq = PP.Perm.Organization.Common.organizationNumberFreq xOrganization

{- | 'xOrganizationByXOrganizationNumber' @n@ returns  the list of pairs associating xOrganization numbers to
the xOrganizations.

>>> mapM_ print $ xOrganizationByXOrganizationNumber 4
(3,[[1,1,1],[1,1,1]])
(4,[[1,2,1],[1,2,1],[1,2,1],[1,2,1]])
(5,[[1,1,3],[1,1,3],[1,3,1],[1,3,1],[2,1,2],[2,1,2],[2,1,2],[2,1,2],[2,1,2],[2,1,2],[3,1,1],[3,1,1]])
(6,[[1,2,3],[1,2,3],[3,2,1],[3,2,1]])
(7,[[2,3,2],[2,3,2]])
-}
xOrganizationByXOrganizationNumber :: Int -> [(Int, [[Int]])]
xOrganizationByXOrganizationNumber =  PP.Perm.Organization.Common.organizationByOrganizationNumber xOrganization