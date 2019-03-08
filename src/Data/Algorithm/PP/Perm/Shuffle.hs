{-|
Module      : Data.Algorithm.PP.Perm.Run
Description : Shuffling permutations
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Shuffling and unshuffling permutations.
-}

module Data.Algorithm.PP.Perm.Shuffle (
    -- * Shuffling
    shuffle
  , shuffle2
  , shuffle3
  , shuffle4

    -- * Unshuffling
  , shuffleOf2
  , shuffleOf2'
  , shuffleOf2''
  ) where

import qualified Control.Arrow as A
import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T

import qualified Data.Algorithm.PP.Perm       as PP.Perm
import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List

{- | 'shuffleOf2' @p@ @q@ @r@ returns @True@ if the permutation @r@ is in the
shuffle of the permutations @p@ and @q@.

>>> let p = mkPerm [1,3,2]; q = mkPerm [3,1,2]; r = mkPerm [1,5,3,6,2,4]) in shuffleOf2 p q r
True
-}
shuffleOf2 :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm -> Bool
shuffleOf2 p q = not . L.null . shuffleOf2' p q

{- | 'shuffleOf2'' @p@ @q@ @r@

>>> let p = mkPerm [1,3,2]; q = mkPerm [3,1,2]; r = mkPerm [1,5,3,6,2,4]) in shuffleOf2' p q r
--[([1,5,3],[6,2,4]),([1,6,2],[5,3,4])]
>>> mk [1,3,2] == mk [1,5,3] && mk [3,1,2] == mk [6,2,4] -- check first solution
True
>>> mk [1,3,2] == mk [1,6,2] && mk [3,1,2] == mk [5,3,4] -- check second solution
True
-}
shuffleOf2' :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm -> [([PP.Geometry.Point.Point], [PP.Geometry.Point.Point])]
shuffleOf2' p q = L.filter (T.uncurry (==) . (mk A.*** mk)) . PP.Perm.partitions np nq
  where
    np = PP.Perm.len p
    nq = PP.Perm.len q

{- | 'shuffleOf2''' @p@ @q@ @r@

>>> let p = mkPerm [1,3,2]; q = mkPerm [3,1,2]; r = mkPerm [1,5,3,6,2,4]) in shuffleOf2'' p q r
Just ([1,5,3],[6,2,4])
-}
shuffleOf2'' :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm -> Maybe ([PP.Geometry.Point.Point], [PP.Geometry.Point.Point])
shuffleOf2'' p q = PP.Utils.List.safeHead . shuffleOf2' p q

{- | 'shuffle2' @p@ @q@ returns all distinct permutations that can be be obtained by shuffling permutation @p@ and @q@.

>>> let p = mk [1,2]; q = mk [2,1] in shuffle2 p q
[[1,3,4,2],[1,3,4,2],[1,3,2,4],[3,1,4,2],[3,1,2,4],[3,1,2,4]]
-}
shuffle2 :: PP.Perm.Perm -> PP.Perm.Perm -> [PP.Perm.Perm]
shuffle2 p q = shuffle [p, q]

{- | 'shuffle3' @p@ @q@ @r@ returns all distinct permutations that can be be obtained by shuffling permutation @p@, @q@ and @r@.

>>> let p = mk [1,2]; q = mk [2,1]; r = mk [2,1] in shuffle3 p q
-}
shuffle3 :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm -> [PP.Perm.Perm]
shuffle3 p q r = shuffle [p, q, r]

{- | 'shuffle4' @p@ @q@ @r@ @s@ returns all distinct permutations that can be be obtained by shuffling permutation @p@, @q@, @r@ and @s@.

>>> let p = mk [1,2]; q = mk [1,2]; r = mk [2,1]; r = mk [2,1] in shuffle4 p q r s
-}
shuffle4 :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm -> [PP.Perm.Perm]
shuffle4 p q r s = shuffle [p, q, r, s]

{- | 'shuffle' @ps@ returns all distinct permutations that can be be obtained by permutations @ps@.

>>> shuffle [mkPerm [1,2], mkPerm [2,1]]
[[1,3,4,2],[1,3,4,2],[1,3,2,4],[3,1,4,2],[3,1,2,4],[3,1,2,4]]
>>> shuffle [mkPerm [1,2], mkPerm [2,1], mkPerm [1,3,2]]
[[1,4,5,2,3,7,6],[1,4,5,2,3,7,6],[1,4,5,2,7,3,6],[1,4,5,2,7,6,3],[1,4,2,5,3,7,6],[1,4,2,5,7,3,6],[1,4,2,5,7,6,3],[1,4,2,7,5,3,6],[1,4,2,7,5,6,3],[1,4,2,7,5,6,3],[1,2,4,5,3,7,6],[1,2,4,5,7,3,6],[1,2,4,5,7,6,3],[1,2,4,7,5,3,6],[1,2,4,7,5,6,3],[1,2,4,7,5,6,3],[1,2,7,4,5,3,6],[1,2,7,4,5,6,3],[1,2,7,4,5,6,3],[1,2,7,4,5,6,3],[1,2,4,5,3,7,6],[1,2,4,5,7,3,6],[1,2,4,5,7,6,3],[1,2,4,7,5,3,6],[1,2,4,7,5,6,3],[1,2,4,7,5,6,3],[1,2,7,4,5,3,6],[1,2,7,4,5,6,3],[1,2,7,4,5,6,3],[1,2,7,4,5,6,3],[1,7,2,4,5,3,6],[1,7,2,4,5,6,3],[1,7,2,4,5,6,3],[1,7,2,4,5,6,3],[1,7,4,2,5,6,3],[1,4,5,2,3,7,6],[1,4,5,2,3,7,6],[1,4,5,2,7,3,6],[1,4,5,2,7,6,3],[1,4,2,5,3,7,6],[1,4,2,5,7,3,6],[1,4,2,5,7,6,3],[1,4,2,7,5,3,6],[1,4,2,7,5,6,3],[1,4,2,7,5,6,3],[1,2,4,5,3,7,6],[1,2,4,5,7,3,6],[1,2,4,5,7,6,3],[1,2,4,7,5,3,6],[1,2,4,7,5,6,3],[1,2,4,7,5,6,3],[1,2,7,4,5,3,6],[1,2,7,4,5,6,3],[1,2,7,4,5,6,3],[1,2,7,4,5,6,3],[1,2,4,5,3,7,6],[1,2,4,5,7,3,6],[1,2,4,5,7,6,3],[1,2,4,7,5,3,6],[1,2,4,7,5,6,3],[1,2,4,7,5,6,3],[1,2,7,4,5,3,6],[1,2,7,4,5,6,3],[1,2,7,4,5,6,3],[1,2,7,4,5,6,3],[1,7,2,4,5,3,6],[1,7,2,4,5,6,3],[1,7,2,4,5,6,3],[1,7,2,4,5,6,3],[1,7,4,2,5,6,3],[1,4,2,5,3,7,6],[1,4,2,3,5,7,6],[1,4,2,3,7,5,6],[1,4,2,3,7,5,6],[1,4,2,3,5,7,6],[1,4,2,3,7,5,6],[1,4,2,3,7,5,6],[1,4,2,7,3,5,6],[1,4,2,7,3,5,6],[1,4,2,7,5,3,6],[1,2,4,3,5,7,6],[1,2,4,3,7,5,6],[1,2,4,3,7,5,6],[1,2,4,7,3,5,6],[1,2,4,7,3,5,6],[1,2,4,7,5,3,6],[1,2,7,4,3,5,6],[1,2,7,4,3,5,6],[1,2,7,4,5,3,6],[1,2,7,4,5,3,6],[1,2,4,3,5,7,6],[1,2,4,3,7,5,6],[1,2,4,3,7,5,6],[1,2,4,7,3,5,6],[1,2,4,7,3,5,6],[1,2,4,7,5,3,6],[1,2,7,4,3,5,6],[1,2,7,4,3,5,6],[1,2,7,4,5,3,6],[1,2,7,4,5,3,6],[1,7,2,4,3,5,6],[1,7,2,4,3,5,6],[1,7,2,4,5,3,6],[1,7,2,4,5,3,6],[1,7,4,2,5,3,6],[4,1,5,2,3,7,6],[4,1,5,2,3,7,6],[4,1,5,2,7,3,6],[4,1,5,2,7,6,3],[4,1,2,5,3,7,6],[4,1,2,5,7,3,6],[4,1,2,5,7,6,3],[4,1,2,7,5,3,6],[4,1,2,7,5,6,3],[4,1,2,7,5,6,3],[4,1,2,5,3,7,6],[4,1,2,5,7,3,6],[4,1,2,5,7,6,3],[4,1,2,7,5,3,6],[4,1,2,7,5,6,3],[4,1,2,7,5,6,3],[4,1,7,2,5,3,6],[4,1,7,2,5,6,3],[4,1,7,2,5,6,3],[4,1,7,5,2,6,3],[1,4,2,5,3,7,6],[1,4,2,5,7,3,6],[1,4,2,5,7,6,3],[1,4,2,7,5,3,6],[1,4,2,7,5,6,3],[1,4,2,7,5,6,3],[1,4,7,2,5,3,6],[1,4,7,2,5,6,3],[1,4,7,2,5,6,3],[1,4,7,5,2,6,3],[1,7,4,2,5,3,6],[1,7,4,2,5,6,3],[1,7,4,2,5,6,3],[1,7,4,5,2,6,3],[1,7,4,5,2,6,3],[4,1,2,5,3,7,6],[4,1,2,3,5,7,6],[4,1,2,3,7,5,6],[4,1,2,3,7,5,6],[4,1,2,3,5,7,6],[4,1,2,3,7,5,6],[4,1,2,3,7,5,6],[4,1,2,7,3,5,6],[4,1,2,7,3,5,6],[4,1,2,7,5,3,6],[4,1,2,3,5,7,6],[4,1,2,3,7,5,6],[4,1,2,3,7,5,6],[4,1,2,7,3,5,6],[4,1,2,7,3,5,6],[4,1,2,7,5,3,6],[4,1,7,2,3,5,6],[4,1,7,2,3,5,6],[4,1,7,2,5,3,6],[4,1,7,5,2,3,6],[1,4,2,3,5,7,6],[1,4,2,3,7,5,6],[1,4,2,3,7,5,6],[1,4,2,7,3,5,6],[1,4,2,7,3,5,6],[1,4,2,7,5,3,6],[1,4,7,2,3,5,6],[1,4,7,2,3,5,6],[1,4,7,2,5,3,6],[1,4,7,5,2,3,6],[1,7,4,2,3,5,6],[1,7,4,2,3,5,6],[1,7,4,2,5,3,6],[1,7,4,5,2,3,6],[1,7,4,5,2,3,6],[4,1,2,5,3,7,6],[4,1,2,3,5,7,6],[4,1,2,3,7,5,6],[4,1,2,3,7,5,6],[4,1,2,3,5,7,6],[4,1,2,3,7,5,6],[4,1,2,3,7,5,6],[4,1,2,7,3,5,6],[4,1,2,7,3,5,6],[4,1,2,7,5,3,6],[4,1,2,3,5,7,6],[4,1,2,3,7,5,6],[4,1,2,3,7,5,6],[4,1,2,7,3,5,6],[4,1,2,7,3,5,6],[4,1,2,7,5,3,6],[4,1,7,2,3,5,6],[4,1,7,2,3,5,6],[4,1,7,2,5,3,6],[4,1,7,5,2,3,6],[1,4,2,3,5,7,6],[1,4,2,3,7,5,6],[1,4,2,3,7,5,6],[1,4,2,7,3,5,6],[1,4,2,7,3,5,6],[1,4,2,7,5,3,6],[1,4,7,2,3,5,6],[1,4,7,2,3,5,6],[1,4,7,2,5,3,6],[1,4,7,5,2,3,6],[1,7,4,2,3,5,6],[1,7,4,2,3,5,6],[1,7,4,2,5,3,6],[1,7,4,5,2,3,6],[1,7,4,5,2,3,6]]
-}
shuffle :: [PP.Perm.Perm] -> [PP.Perm.Perm]
shuffle = L.map PP.Perm.mk . PP.Utils.List.shuffle . L.map PP.Perm.getList
