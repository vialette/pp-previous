module Data.Algorithm.PP.SeparatingTree (
  -- * Type
    SeparatingTree(..)

  -- * Constructing
  , mk

  -- * Querying
  , getPerm
) where

import qualified Data.Algorithm.PP.Perm as PP.Perm

-- |Separating tree type definition
data SeparatingTree = PlusNode SeparatingTree SeparatingTree
                    | MinusNode SeparatingTree SeparatingTree
                    | Leaf Int
                    deriving (Show)


{- | 'mk' @p@ returns a separating tree of the permutation @p@ if it is separable.
-- Otherwise, the functions returns 'Nothing'.

>>> import qualified Data.Algorithm.PP.SeparatingTree as ST
>>> ST.mk $ mk [1,2,4,3]
Just (MinusNode (MinusNode (Leaf 4) (Leaf 3)) (PlusNode (Leaf 1) (Leaf 2)))
>>> ST.mk $ mk [3,4,2,1]
Just (MinusNode (MinusNode (PlusNode (Leaf 3) (Leaf 4)) (Leaf 2)) (Leaf 1))
>>> ST.mk $ mk [2,4,1,3]
Nothing
>>> ST.mk $ mk [3,1,4,2]
Nothing
-}
mk :: PP.Perm.Perm -> Maybe SeparatingTree
mk = go [] . PP.Perm.getList
  where
    go s  [] = go' s
    go [] (x : xs) = go [(x, Leaf x, x)] xs
    go s'@((yMin, t, yMax) : s) (x : xs)
      | x == yMax+1 = go ((yMin, PlusNode  t (Leaf x), x) : s)    xs
      | x == yMin-1 = go ((x,    MinusNode t (Leaf x), yMax) : s) xs
      | otherwise     = go ((x, Leaf x, x) : s')                  xs

    go' [] = Nothing
    go' [(_, t, _)] = Just t
    go' ((yMin, t, yMax) : (yMin', t', yMax') : s)
      | yMax +1 == yMin' = go' ((yMin,  PlusNode t t',  yMax') : s)
      | yMax'+1 == yMin  = go' ((yMin', MinusNode t t', yMax) : s)
      | otherwise          = Nothing

{- | 'getPerm' @t@ returns the permutations associated to the separating tree @t@.

-}
getPerm :: SeparatingTree -> PP.Perm.Perm
getPerm = PP.Perm.mkUnsafe . aux []
  where
    aux acc (Leaf i)          = i : acc
    aux acc (PlusNode lt rt)  = let acc' = aux acc rt in aux acc' lt
    aux acc (MinusNode lt rt) = let acc' = aux acc lt in aux acc' lt
