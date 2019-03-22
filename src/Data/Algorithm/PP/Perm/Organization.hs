{-|
Module      : Data.Algorithm.PP.Perm.Pattern
Description : Organization numbers
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

The /organization list/ of a permutation \(\pi = \pi_{1} \pi_{2} \dots \pi_{n}\) of \([n]\) is
the list \(o(\pi) = (o_{1}, o_{2}, \dots, o_{n-1})\) defined by \(o_{i} = | \pi_{i+1} - \pi_{i} |\)
for \(1 \leq i \leq n-1\).
For example the organization list of \(\pi = 5 2 1 3 4\) is \(o(\pi) = (3, 1, 2, 1)\).

The /organization number/ of a permutation \(\pi = \pi_{1} \pi_{2} \dots \pi_{n}\) of \([n]\) is
the integer \(\sum_{i=1}^{n-1} o_{i}\),
where \(o(\pi) = (o_{1}, o_{2}, \dots, o_{n-1})\) is the organization list of \(\pi\).

The maximal value of the organization number of any permutation of \([n]\)
for \(n = 0, 1, 2, 3, \dots\) is given by \(0, 1, 3, 7, 11, 17, 23, \dots\)
(sequence [A047838](https://oeis.org/A047838)).
-}

module Data.Algorithm.PP.Perm.Organization (
  -- * Type
    OrganizationList

  -- * Lists
  , organizationList
  , organizationLists
  , organizationListAssoc
  , organizationListByOrganizationNumber
  , organizationListFreq
  , distinctOrganizationLists
  , maxOrganizationLists

  -- * Numbers
  , organizationNumber
  , organizationNumbers
  , maxOrganizationNumber
  , maxOrganizationNumbers
  , organizationNumberAssoc
  , organizationNumberFreq
  , distinctOrganizationNumbers

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
type OrganizationList = [Int]

{- | 'organizationList' @p@ returns the organization list of permutation @p@.

>>> mapM_ print . fmap (id &&& organizationList) $ perms 4
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
organizationList :: PP.Perm.Perm -> OrganizationList
organizationList = L.map (abs . uncurry (-)) . PP.Utils.List.chunk2 . PP.Perm.getList

{- | 'organizationLists' @n@ returns the organization lists of all permutations of length @n@.
Duplicates organization lists are not removed.

>>> organizationLists 4
    [[1,1,1],[1,2,1],[1,1,3],[1,2,3],[2,1,2],[2,1,2],[1,1,1],[1,2,1],[1,2,3],[2,1,2],[2,1,2],[1,1,3],[3,1,1],[3,2,1],[1,2,1],[2,1,2],[2,3,2],[1,3,1],[3,2,1],[3,1,1],[2,1,2],[1,2,1],[1,3,1],[2,3,2]]
-}
organizationLists :: Int -> [OrganizationList]
organizationLists = L.map organizationList . PP.Perm.Generator.Basic.perms

{- | 'organizationListAssoc' @n@ returns the list of pairs associating organization lists to
permutations.

>>> mapM_ print $ organizationListAssoc 4
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
organizationListAssoc :: Int -> [(OrganizationList, [PP.Perm.Perm])]
organizationListAssoc = L.map g . L.groupBy f . L.sort . L.map (organizationList &&& id) . PP.Perm.Generator.Basic.perms
  where
    f t t' = T.fst t == T.fst t'
    g      = (T.fst . L.head) &&& (L.map T.snd)

{- | 'organizationListByOrganizationNumber' @n@ returns  the list of pairs associating organization numbers to
the organization lists.

>>> mapM_ print $ organizationListByOrganizationNumber 4
(3,[[1,1,1],[1,1,1]])
(4,[[1,2,1],[1,2,1],[1,2,1],[1,2,1]])
(5,[[1,1,3],[1,1,3],[1,3,1],[1,3,1],[2,1,2],[2,1,2],[2,1,2],[2,1,2],[2,1,2],[2,1,2],[3,1,1],[3,1,1]])
(6,[[1,2,3],[1,2,3],[3,2,1],[3,2,1]])
(7,[[2,3,2],[2,3,2]])
-}
organizationListByOrganizationNumber :: Int -> [(Int, [OrganizationList])]
organizationListByOrganizationNumber =  L.map g . L.groupBy f . L.sort . L.map (F.sum &&& id) . organizationLists
  where
    f t t' = T.fst t == T.fst t'
    g      = (T.fst . L.head) &&& (L.map T.snd)

{- | 'organizationListFreq' @n@ returns the list of pairs associating organization lists to
the number of permutations.

>>> mapM_ print $ organizationListFreq 4
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
organizationListFreq :: Int -> [(OrganizationList, Int)]
organizationListFreq = L.map (id *** L.length) . organizationListAssoc

{- | 'distinctOrganizationLists'

>>> distinctOrganizationLists 4
[[1,1,1],[1,1,3],[1,2,1],[1,2,3],[1,3,1],[2,1,2],[2,3,2],[3,1,1],[3,2,1]]
-}
distinctOrganizationLists :: Int -> [OrganizationList]
distinctOrganizationLists = L.sort . PP.Utils.List.uniq . organizationLists

{- | 'maxOrganizationLists'

>>>  maxOrganizationLists 2
(1,[[1]])
>>> maxOrganizationLists 3
(3,[[1,2],[2,1]])
>>> maxOrganizationLists 4
(7,[[2,3,2]])
>>> maxOrganizationLists 5
(11,[[1,3,4,3],[2,3,4,2],[2,4,3,2],[3,4,3,1]])
-}
maxOrganizationLists :: Int -> (Int, [OrganizationList])
maxOrganizationLists = second (L.map T.fst) . PP.Utils.Foldable.maximumBy T.snd 0 [] . L.map (id &&& F.sum) . distinctOrganizationLists

{- | 'organizationNumber'

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
organizationNumber = F.sum . organizationList

{- | 'organizationNumbers'

>>> organizationNumbers 4
[3,4,5,6,5,5,3,4,6,5,5,5,5,6,4,5,7,5,6,5,5,4,5,7]
-}
organizationNumbers :: Int -> [Int]
organizationNumbers = L.map organizationNumber.  PP.Perm.Generator.Basic.perms

{- | 'maxOrganizationNumber'

>>> maxOrganizationNumber 2
1
>>> maxOrganizationNumber 3
3
>>> maxOrganizationNumber 4
7
>>> maxOrganizationNumber 5
11
-}
maxOrganizationNumber :: Int -> Int
maxOrganizationNumber = F.maximum . organizationNumbers

{- | 'maxOrganizationNumbers'

>>> take 10 $ maxOrganizationNumbers
[0,1,3,7,11,17,23,31,39,49]
-}
maxOrganizationNumbers :: [Int]
maxOrganizationNumbers = [maxOrganizationNumber n | n <- [1..]]

{- | 'organizationListAssoc'

>>> mapM_ print $ organizationNumberAssoc 4
(3,[[1,2,3,4],[4,3,2,1]])
(4,[[1,2,4,3],[2,1,3,4],[3,4,2,1],[4,3,1,2]])
(5,[[1,3,2,4],[1,3,4,2],[1,4,3,2],[2,1,4,3],[2,3,4,1],[2,4,3,1],[3,1,2,4],[3,2,1,4],[3,4,1,2],[4,1,2,3],[4,2,1,3],[4,2,3,1]])
(6,[[1,4,2,3],[2,3,1,4],[3,2,4,1],[4,1,3,2]])
(7,[[2,4,1,3],[3,1,4,2]])
-}
organizationNumberAssoc :: Int -> [(Int, [PP.Perm.Perm])]
organizationNumberAssoc = L.map g . L.groupBy f . L.sort . L.map (organizationNumber &&& id) . PP.Perm.Generator.Basic.perms
  where
    f t t' = T.fst t == T.fst t'
    g      = (T.fst . L.head) &&& (L.map T.snd)

{- | 'organizationListFreq' @n@ returns the list of pairs associating the organization numbers to the number of
permutation having this organization number.

>>> mapM_ print $ organizationNumberFreq 4
(3,2)
(4,4)
(5,12)
(6,4)
(7,2)
-}
organizationNumberFreq :: Int -> [(Int, Int)]
organizationNumberFreq = L.map (id *** L.length) . organizationNumberAssoc

{- | 'distinctOrganizationNumbers'

>>> distinctOrganizationNumbers 4
[3,4,5,6,7]
-}
distinctOrganizationNumbers :: Int -> [Int]
distinctOrganizationNumbers =  L.sort . PP.Utils.List.uniq . organizationNumbers

