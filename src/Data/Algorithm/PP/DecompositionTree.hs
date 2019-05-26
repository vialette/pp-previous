module Data.Algorithm.PP.DecompositionTree
  (
    -- * Type
      DecompositionTree(..)

    -- * Constructing
    , mk

    -- * Querying
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

    reduce s@((t1, i1) : (t2, i2) : s')
      | i2Max+1 == i1Min = tPlus
      | i1Max+1 == i2Min = tMinus
      | otherwise        = s
      where
        tPlus  = reduce ((BranchPlus  t2 t1, (i2Min, i1Max)) : s')
        tMinus = reduce ((BranchMinus t2 t1, (i1Min, i2Max)) : s')

    reduce s = s
