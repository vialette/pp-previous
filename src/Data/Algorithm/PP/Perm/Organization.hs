{-|
Module      : Data.Algorithm.PP.Perm.Pattern
Description : Organization numbers
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

The /organization/ of a permutation \(\pi = \pi_{1} \pi_{2} \dots \pi_{n}\) of \([n]\) is
the list \(o(\pi) = (o_{1}, o_{2}, \dots, o_{n-1})\) defined by \(o_{i} = | \pi_{i+1} - \pi_{i} |\)
for \(1 \leq i \leq n-1\).
For example the organization of \(\pi = 5 2 1 3 4\) is \(o(\pi) = (3, 1, 2, 1)\).

The /organization number/ of a permutation \(\pi = \pi_{1} \pi_{2} \dots \pi_{n}\) of \([n]\) is
the integer \(\sum_{i=1}^{n-1} o_{i}\),
where \(o(\pi) = (o_{1}, o_{2}, \dots, o_{n-1})\) is the organization of \(\pi\).

The maximal value of the organization number of any permutation of \([n]\)
for \(n = 0, 1, 2, 3, \dots\) is given by \(0, 1, 3, 7, 11, 17, 23, \dots\)
(sequence [A047838](https://oeis.org/A047838)).
-}

module Data.Algorithm.PP.Perm.Organization (
  -- * Type
    Organization

  -- * Lists
  , organization
  , organizations
  , organizationAssoc
  , organizationByOrganizationNumber
  , organizationFreq

  -- * Numbers
  , organizationNumber
  , organizationNumbers
  , organizationNumberAssoc
  , organizationNumberFreq
  ) where

import Control.Arrow
import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T

import qualified Data.Algorithm.PP.Utils.Foldable       as PP.Utils.Foldable
import qualified Data.Algorithm.PP.Utils.List           as PP.Utils.List
import qualified Data.Algorithm.PP.Perm                 as PP.Perm
import qualified Data.Algorithm.PP.Perm.Generator.Basic as PP.Perm.Generator.Basic

{- | Type alias -}
type Organization = [Int]

{- | 'organization' @p@ returns the organization of permutation @p@.

>>> mapM_ print . fmap (id &&& organization) $ perms 4
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
organization :: PP.Perm.Perm -> Organization
organization = L.map (abs . uncurry (-)) . PP.Utils.List.chunk2 . PP.Perm.getList

{- | 'organizations' @n@ returns the organizations of all permutations of length @n@.
Duplicates organizations are not removed.

>>> organizations 4
[[1,1,1],[1,2,1],[1,1,3],[1,2,3],[2,1,2],[2,1,2],[1,1,1],[1,2,1],[1,2,3],[2,1,2],[2,1,2],[1,1,3],[3,1,1],[3,2,1],[1,2,1],[2,1,2],[2,3,2],[1,3,1],[3,2,1],[3,1,1],[2,1,2],[1,2,1],[1,3,1],[2,3,2]]
-}
organizations :: Int -> [Organization]
organizations = L.map organization . PP.Perm.Generator.Basic.perms

{- | 'organizationAssoc' @ps@ returns the list of pairs associating organizations to
permutations in @xs@.

>>> mapM_ print . organizationAssoc $ perms 4
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
organizationAssoc :: [PP.Perm.Perm] -> [(Organization, [PP.Perm.Perm])]
organizationAssoc = L.map g . L.groupBy f . L.sort . L.map (organization &&& id)
  where
    f t t' = T.fst t == T.fst t'
    g      = (T.fst . L.head) &&& (L.map T.snd)

{- | 'organizationFreq' @n@ returns the list of pairs associating organizations to
the number of permutations.

>>> mapM_ print . organizationFreq $ perms 4
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
organizationFreq :: [PP.Perm.Perm] -> [(Organization, Int)]
organizationFreq = L.map (id *** L.length) . organizationAssoc

{- | 'organizationNumber' @p@ returns the organization number of the permutation @p@.
The organization number of a permutation is the sum of the integers of the corresponding organization.

>>> mapM_ print . fmap (id &&& organizationNumber) $ perms 4
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
organizationNumber :: PP.Perm.Perm -> Int
organizationNumber = F.sum . organization

{- | 'organizationNumbers' @n@ returns the organization numbers of all permutations of length @n@.
Duplicates organizations are not removed.

>>> organizationNumbers 3
>>> organizationNumbers 4
[[1,1,1],[1,2,1],[1,1,3],[1,2,3],[2,1,2],[2,1,2],[1,1,1],[1,2,1],[1,2,3],[2,1,2],[2,1,2],[1,1,3],[3,1,1],[3,2,1],[1,2,1],[2,1,2],[2,3,2],[1,3,1],[3,2,1],[3,1,1],[2,1,2],[1,2,1],[1,3,1],[2,3,2]]
-}
organizationNumbers :: Int -> [Organization]
organizationNumbers = fmap organization . PP.Perm.Generator.Basic.perms

{- | 'organizationNumberAssoc'


-}
organizationNumberAssoc :: [PP.Perm.Perm] -> [(Int, [PP.Perm.Perm])]
organizationNumberAssoc = L.map g . L.groupBy f . L.sort . L.map (organizationNumber &&& id)
  where
    f t t' = T.fst t == T.fst t'
    g      = (T.fst . L.head) &&& (L.map T.snd)

{- | 'organizationFreq' @n@ returns the list of pairs associating the organization numbers to the number of
permutation having this organization number.

>>> mapM_ print $ organizationNumberFreq 4
(3,2)
(4,4)
(5,12)
(6,4)
(7,2)
-}
organizationNumberFreq :: [PP.Perm.Perm] -> [(Int, Int)]
organizationNumberFreq = L.map (id *** L.length) . organizationNumberAssoc

{- | 'organizationByOrganizationNumber' @n@ returns  the list of pairs associating organization numbers to
the organizations.

>>> mapM_ print $ organizationByOrganizationNumber 4
(3,[[1,1,1],[1,1,1]])
(4,[[1,2,1],[1,2,1],[1,2,1],[1,2,1]])
(5,[[1,1,3],[1,1,3],[1,3,1],[1,3,1],[2,1,2],[2,1,2],[2,1,2],[2,1,2],[2,1,2],[2,1,2],[3,1,1],[3,1,1]])
(6,[[1,2,3],[1,2,3],[3,2,1],[3,2,1]])
(7,[[2,3,2],[2,3,2]])
-}
organizationByOrganizationNumber :: Int -> [(Int, [Organization])]
organizationByOrganizationNumber =  L.map g . L.groupBy f . L.sort . L.map (F.sum &&& id) . organizations
  where
    f t t' = T.fst t == T.fst t'
    g      = (T.fst . L.head) &&& (L.map T.snd)