module Data.Algorithm.PP.Perm.Organization.Common (
    -- * List
      organizations
    , organizationAssoc
    , organizationByOrganizationNumber
    , organizationFreq

    -- * Number
    , organizationNumber
    , organizationNumbers
    , organizationNumberAssoc
    , organizationNumberFreq
  ) where

import Control.Arrow
import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T

import qualified Data.Algorithm.PP.Utils.Foldable           as PP.Utils.Foldable
import qualified Data.Algorithm.PP.Utils.List               as PP.Utils.List
import qualified Data.Algorithm.PP.Perm                     as PP.Perm
import qualified Data.Algorithm.PP.Perm.Generator.Basic     as PP.Perm.Generator.Basic


organizations :: (PP.Perm.Perm -> [Int]) -> Int -> [[Int]]
organizations f = L.map f . PP.Perm.Generator.Basic.perms

organizationAssoc :: (PP.Perm.Perm -> [Int]) -> [PP.Perm.Perm] -> [([Int], [PP.Perm.Perm])]
organizationAssoc f = L.map h . L.groupBy g . L.sort . L.map (f &&& id)
  where
    g t t' = T.fst t == T.fst t'
    h      = (T.fst . L.head) &&& (L.map T.snd)

organizationFreq :: (PP.Perm.Perm -> [Int]) -> [PP.Perm.Perm] -> [([Int], Int)]
organizationFreq f = L.map (id *** L.length) . organizationAssoc f

organizationNumber :: (PP.Perm.Perm -> [Int]) -> PP.Perm.Perm -> Int
organizationNumber f = F.sum . f

organizationNumbers :: (PP.Perm.Perm -> [Int]) -> Int -> [[Int]]
organizationNumbers f = fmap f . PP.Perm.Generator.Basic.perms

organizationNumberAssoc :: (PP.Perm.Perm -> [Int]) -> [PP.Perm.Perm] -> [(Int, [PP.Perm.Perm])]
organizationNumberAssoc f = L.map h . L.groupBy g . L.sort . L.map (f &&& id)
  where
    g t t' = T.fst t == T.fst t'
    h      = (T.fst . L.head) &&& (L.map T.snd)

organizationNumberFreq :: (PP.Perm.Perm -> [Int]) -> [PP.Perm.Perm] -> [(Int, Int)]
organizationNumberFreq f = L.map (id *** L.length) . organizationNumberAssoc f


organizationByXOrganizationNumber :: (PP.Perm.Perm -> [Int]) -> Int -> [(Int, [[Int]])]
organizationByXOrganizationNumber f =  L.map h . L.groupBy g . L.sort . L.map (F.sum &&& id) . organizations f
  where
    g t t' = T.fst t == T.fst t'
    h      = (T.fst . L.head) &&& (L.map T.snd)