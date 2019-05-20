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

import qualified Data.Algorithm.PP.Perm as PP.Perm

-- |Separating tree type definition
data SeparatingTree = PlusNode SeparatingTree SeparatingTree
                    | MinusNode SeparatingTree SeparatingTree
                    | Leaf Int
                    deriving (Show)

{- | 'mk' @p@ returns a separating tree of the permutation @p@ if it is separable.
-- Otherwise, the functions returns 'Nothing'.

>>> import qualified Data.Algorithm.PP.SeparatingTree as ST
>>> let p = mk [1,2,4,3] in ST.mk
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
    go stack  [] = case stack of
                     [(_, t, _)] -> Just t
                     _           -> Nothing
    go [] (x : xs) = go [(x, Leaf x, x)] xs
    go stack@((yMin, t, yMax) : nextStack) (x : xs)
      | x == yMax+1 = let newStack = reduce ((yMin, PlusNode  t (Leaf x), x) : nextStack)
                      in go newStack xs
      | x == yMin-1 = let newStack = reduce ((x, MinusNode t (Leaf x), yMax) : nextStack)
                      in go newStack xs
      | otherwise   = go ((x, Leaf x, x) : stack) xs

    reduce [] = []
    reduce [(yMin, t, yMax)] = [(yMin, t, yMax)]
    reduce ((yMin, t, yMax) : (yMin', t', yMax') : nextStack)
      | yMax +1 == yMin' = reduce ((yMin,  PlusNode t t',  yMax') : nextStack)
      | yMax'+1 == yMin  = reduce ((yMin', MinusNode t t', yMax) : nextStack)
      | otherwise        = []

{- | 'getPerm' @t@ returns the permutations associated to the separating tree @t@.

-}
getPerm :: SeparatingTree -> PP.Perm.Perm
getPerm = PP.Perm.mkUnsafe . aux []
  where
    aux acc (Leaf i)          = i : acc
    aux acc (PlusNode lt rt)  = let acc' = aux acc rt in aux acc' lt
    aux acc (MinusNode lt rt) = let acc' = aux acc lt in aux acc' lt
