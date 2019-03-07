module Data.Algorithm.PP.Perm.Combinator (
    -- * Direct sum
    (<<+>>)
  , directSum

    -- * Skew sum
  , (<<->>)
  , skewSum

    -- * Dot product
  , (<<.>>)
  , dotProduct
  ) where

import Control.Applicative ((<$>))
import Control.Monad (foldM)
import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T
import Data.Function (on)

import qualified Data.Algorithm.PP.Utils.Maybe as PP.Utils.Maybe
import qualified Data.Algorithm.PP.Perm        as PP.Perm

{- | @p@ '<<+>>' @q@ returns the direct sum of the permutations @p@ and @q@.

>>> mkPerm [2,4,1,3] <<+>> mkPerm [3,5,1,4,2]
[2,4,1,3,7,9,5,8,6]
-}
(<<+>>) :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm
p <<+>> q = PP.Perm.mkPerm (xs ++ ys)
  where
    xs = PP.Perm.getList p
    n  = L.length xs
    ys = (+ n) <$> PP.Perm.getList q

{- | 'directSum' @ps@ returns the direct sum of the permutations @ps@.

>>> directSum [mkPerm [i,i-1..1] | i <- [1..5]]
[1,3,2,6,5,4,10,9,8,7,15,14,13,12,11]
-}
directSum :: (Foldable t) => t PP.Perm.Perm -> PP.Perm.Perm
directSum = aux . F.toList
  where
    aux [] = PP.Perm.empty
    aux ps = F.foldr1 (<<+>>) ps

{- | @p@ '<<->>' @q@ returns the skew sum of the permutations @p@ and @q@.

>>> mkPerm [2,4,1,3] <<->> mkPerm [3,5,1,4,2]
[7,9,6,8,3,5,1,4,2]
-}
(<<->>) :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm
p <<->> q = PP.Perm.mkPerm (xs ++ ys)
  where
    n  = L.length ys
    xs = L.map (+ n) $ PP.Perm.getList p
    ys = PP.Perm.getList q

{- | 'skewSum' @ps@ returns the skew sums of the permutations @ps@.

>>> skewSum [mkPerm [i,i-1..1] | i <- [1..5]]
[15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
-}
skewSum :: (Foldable t) => t PP.Perm.Perm -> PP.Perm.Perm
skewSum = aux . F.toList
  where
    aux [] = PP.Perm.empty
    aux ps = F.foldr1 (<<->>) ps

{- | @p@ '<<.>>' @q@ returns the dot product of the permutations @p@ and @q@.

>>> mkPerm [4,2,3,1] <<.>> mkPerm [1,3,2,4]
Just [4,3,2,1]
-}
(<<.>>) :: PP.Perm.Perm -> PP.Perm.Perm -> Maybe PP.Perm.Perm
p <<.>> q = PP.Utils.Maybe.whenMaybe (PP.Perm.len p == PP.Perm.len q)
              (PP.Perm.mkPerm . L.map T.snd . L.sortBy cmpFst . L.zipWith (T.curry proj) ips $ L.sortBy cmpSnd iqs)
  where
    cmpFst      = compare `on` T.fst
    cmpSnd      = compare `on` T.snd
    proj (x, y) = (T.fst y, T.snd x)
    ips         = L.zip [1..] $ PP.Perm.getList p
    iqs         = L.zip [1..] $ PP.Perm.getList q

{- | 'dotProduct' @ps@ returns the dot product of the permutations @ps@.

>>> dotProduct [mkPerm [1,4,2,3], mkPerm [4,3,2,1], mkPerm [4,1,3,2]]
Just [1,3,4,2]
>>> dotProduct [mkPerm [1..3], mkPerm [1..4]]
Nothing
-}
dotProduct :: (Foldable t) => t PP.Perm.Perm -> Maybe PP.Perm.Perm
dotProduct = aux . F.toList
  where
    aux []       = Nothing
    aux (p : ps) = foldM (<<.>>) (PP.Perm.identity n) (p : ps)
      where
        n = PP.Perm.len p
