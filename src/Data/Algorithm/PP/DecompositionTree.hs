module Data.Algorithm.PP.DecompositionTree
  (
    -- * Type
      DecompositionTree(..)

    -- * Constructing
    , mk

    -- * Querying
    , getPerm
  ) where

import qualified Data.List as L

import qualified Data.Algorithm.PP.Perm       as PP.Perm
import qualified Data.Algorithm.PP.Perm.Small as PP.Perm.Small

data DecompositionTree = BranchPlus  DecompositionTree DecompositionTree
                       | BranchMinus DecompositionTree DecompositionTree
                       | Branch2413  DecompositionTree DecompositionTree DecompositionTree DecompositionTree
                       | Branch3142  DecompositionTree DecompositionTree DecompositionTree DecompositionTree
                       | Leaf Int
                       deriving (Show)

mk :: PP.Perm.Perm -> Maybe DecompositionTree
mk = aux [] . PP.Perm.getList
  where
    aux [(t, _)] []       = Just t
    aux _        []       = Nothing
    aux s        (x : xs) = aux s' xs
      where
        s' = reduce ((Leaf x, (x, x)) : s)

    reduce s@((t1, (i1Min, i1Max)) : (t2, (i2Min, i2Max)) : (t3, (i3Min, i3Max)) : (t4, (i4Min, i4Max)) : s')
      | i2Max+1 == i1Min = tPlus
      | i1Max+1 == i2Min = tMinus
      | i2Max+1 == i4Min && i4Max+1 == i1Min && i1Max+1 == i3Min = t2413
      | i3Max+1 == i1Min && i1Max+1 == i4Min && i4Max+1 == i2Min = t3142
      | otherwise = s
       where
        tPlus  = reduce ((BranchPlus  t2 t1, (i2Min, i1Max)) : (t3, (i3Min, i3Max)) : (t4, (i4Min, i4Max)) : s')
        tMinus = reduce ((BranchMinus t2 t1, (i1Min, i2Max)) : (t3, (i3Min, i3Max)) : (t4, (i4Min, i4Max)) : s')
        t2413  = reduce ((Branch2413  t4 t3 t2 t1, (i2Min, i3Max)) : s')
        t3142  = reduce ((Branch3142  t4 t3 t2 t1, (i3Min, i2Max)) : s')

    reduce s@((t1, (i1Min, i1Max)) : (t2, (i2Min, i2Max)) : s')
      | i2Max+1 == i1Min = tPlus
      | i1Max+1 == i2Min = tMinus
      | otherwise        = s
      where
        tPlus  = reduce ((BranchPlus  t2 t1, (i2Min, i1Max)) : s')
        tMinus = reduce ((BranchMinus t2 t1, (i1Min, i2Max)) : s')

    reduce s = s

{- | 'getPerm' @t@ returns the permutations associated to the decomposition tree @t@.

>>> let p = mk [1,5,3,4,2] in DT.mk p >>= (Just . DT.getPerm)
Just [1,5,3,4,2]
>>> let p = mk [3,2,5,4,2] in DT.mk p >>= (Just . DT.getPerm)
Just [3,1,5,4,2]
>>> let p = mk [3,5,1,4,2] in ST.mk p >>= (Just . DT.getPerm)
Nothing
-}
getPerm :: DecompositionTree -> PP.Perm.Perm
getPerm = PP.Perm.mk . aux []
  where
    aux acc (Leaf i)                 = i : acc
    aux acc (BranchPlus lt rt)       = acc''
      where
        acc'  = aux acc  rt
        acc'' = aux acc' lt
    aux acc (BranchMinus lt rt)      = acc''
      where
        acc'  = aux acc  rt
        acc'' = aux acc' lt
    aux acc (Branch2413 t2 t4 t1 t3) = acc2
      where
        acc3 = aux acc  t3
        acc1 = aux acc3 t1
        acc4 = aux acc1 t4
        acc2 = aux acc4 t2
    aux acc (Branch3142 t3 t1 t4 t2) = acc3
      where
        acc2 = aux acc  t2
        acc4 = aux acc2 t4
        acc1 = aux acc4 t1
        acc3 = aux acc1 t3