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
mk = aux [] . PP.Perm.getList
  where
    aux stack  [] = case stack of
                     [(t, _)] -> Just t
                     _        -> Nothing
    aux [] (x : xs)  = let i = PP.Interval.mk x x
                           n = Leaf x
                           newStack = push (n, i) emptyStack
                      in aux newStack xs
    aux stack@((t, i) : nextStack) (x : xs)
      | x == iMax+1 = let i' = PP.Interval.mk iMin x
                          n = PlusNode  t (Leaf x)
                          newStack = push (n, i') nextStack
                          reducedNewStack = reduce newStack
                      in aux reducedNewStack xs
      | x == iMin-1 = let i' = PP.Interval.mk x iMax
                          n =  MinusNode t (Leaf x)
                          newStack = push (n, i') nextStack
                          reducedNewStack = reduce newStack
                      in aux reducedNewStack xs
      | otherwise   = let i = PP.Interval.mk x x
                          n =  Leaf x
                          newStack = push (n, i) stack
                      in aux newStack xs
      where
        iMin  = PP.Interval.getLeft i
        iMax  = PP.Interval.getRight i

    -- reduce top stack elements.
    reduce :: [(SeparatingTree, PP.Interval.Interval)] -> [(SeparatingTree, PP.Interval.Interval)]
    reduce [] = []
    reduce [(i, t)] = [(i, t)]
    reduce stack@((t, i) : (t', i') : nextStack)
      | iMax+1 == iMin' = let i'' = PP.Interval.mk iMin iMax'
                              n = PlusNode t t'
                              newStack = push (n, i'') nextStack
                          in reduce newStack
      | iMax'+1 == iMin = let i'' = PP.Interval.mk  iMin' iMax
                              n = MinusNode t t'
                              newStack = push (n, i'') nextStack
                          in reduce newStack
      | otherwise       = stack
      where
        iMin  = PP.Interval.getLeft i
        iMax  = PP.Interval.getRight i
        iMin' = PP.Interval.getLeft i'
        iMax' = PP.Interval.getRight i'

    -- empty stack
    emptyStack :: [(SeparatingTree, PP.Interval.Interval)]
    emptyStack = []

    -- push an element onto the stack
    push :: (SeparatingTree, PP.Interval.Interval) -> [(SeparatingTree, PP.Interval.Interval)] -> [(SeparatingTree, PP.Interval.Interval)]
    push x stack = x : stack

{- | 'getPerm' @t@ returns the permutations associated to the separating tree @t@.

-}
getPerm :: SeparatingTree -> PP.Perm.Perm
getPerm = PP.Perm.mkUnsafe . aux []
  where
    aux acc (Leaf i)          = i : acc
    aux acc (PlusNode lt rt)  = let acc' = aux acc rt in aux acc' lt
    aux acc (MinusNode lt rt) = let acc' = aux acc lt in aux acc' lt
