module Data.Algorithm.PP.SeparatingTree
  (
    -- * Type
      SeparatingTree(..)

    -- * Constructing
    , mk

    -- * Querying
    , getPerm
  ) where

import qualified Data.List as L

import qualified Data.Algorithm.PP.Interval as PP.Interval
import qualified Data.Algorithm.PP.Perm     as PP.Perm

-- |Separating tree type definition
data SeparatingTree = BranchPlus SeparatingTree SeparatingTree
                    | BranchMinus SeparatingTree SeparatingTree
                    | Leaf Int
                    deriving (Show)

{- | 'mk' @p@ returns a separating tree of the permutation @p@ if it is separable.
-- Otherwise, the functions returns 'Nothing'.

>>> import qualified Data.Algorithm.PP.SeparatingTree as ST
>>> let p = mk [1,2,4,3] in ST.mk
Just (BranchMinus (BranchMinus (Leaf 4) (Leaf 3)) (BranchPlus (Leaf 1) (Leaf 2)))
>>> ST.mk $ mk [3,4,2,1]
Just (BranchMinus (BranchMinus (BranchPlus (Leaf 3) (Leaf 4)) (Leaf 2)) (Leaf 1))
>>> ST.mk $ mk [2,4,1,3]
Nothing
>>> ST.mk $ mk [3,1,4,2]
Nothing
-}
mk :: PP.Perm.Perm -> Maybe SeparatingTree
mk = aux [] . PP.Perm.getList
  where
    aux [(t, _)] []       = Just t
    aux _        []       = Nothing
    aux s        (x : xs) = aux s' xs
      where
        s' = reduce ((Leaf x, (x, x)) : s)

    reduce s@((t1, i1) : (t2, i2) : s')
      | i2Max+1 == i1Min = tPlus
      | i1Max+1 == i2Min = tMinus
      | otherwise        = s
      where
        tPlus  = reduce ((BranchPlus  t2 t1, (i2Min, i1Max)) : s')
        tMinus = reduce ((BranchMinus t2 t1, (i1Min, i2Max)) : s')

    reduce s = s

{- | 'getPerm' @t@ returns the permutations associated to the separating tree @t@.

-}
getPerm :: SeparatingTree -> PP.Perm.Perm
getPerm = PP.Perm.mkUnsafe . aux []
  where
    aux acc (Leaf i)                 = i : acc
    aux acc (BranchPlus lt rt)       = acc''
      where
        acc'  = aux acc  lt
        acc'' = aux acc' rt
    aux acc (BranchMinus lt rt)      = acc''
      where
        acc'  = aux acc  lt
        acc'' = aux acc' rt
