module Data.Algorithm.PP.DecompositionTree
  (
    -- * Type
      DecompositionTree(..)

    -- * Constructing
    , mk

    -- * Querying
--    , getPerm
  ) where

import qualified Data.List as L

import qualified Data.Algorithm.PP.Perm as PP.Perm

data DecompositionTree = BranchPlus  DecompositionTree DecompositionTree
                       | BranchMinus DecompositionTree DecompositionTree
                       | Branch2413  DecompositionTree DecompositionTree DecompositionTree DecompositionTree
                       | Branch3142  DecompositionTree DecompositionTree DecompositionTree DecompositionTree
                       | Leaf Int
                       deriving (Show)

mk :: PP.Perm.Perm -> DecompositionTree
mk _ = Leaf 0
--mk = aux [] . PP.Perm.getList
--  where
--    aux stack  [] = case stack of
--                     [(t, _)] -> Just t
--                     _           -> Nothing
--    aux [] (x : xs)  = let i = PP.Interval.mk x x
--                           n = Leaf x
--                           newStack = push (n, i) emptyStack
--                      in aux newStack xs
--    aux stack@((t, i) : nextStack) (x : xs)
--      | x == iMax+1 = let i' = PP.Interval.mk iMin x
--                          n = PlusNode  t (Leaf x)
--                          newStack = push (n, i') nextStack
--                          reducedNewStack = reduce newStack
--                      in aux reducedNewStack xs
--      | x == iMin-1 = let i' = PP.Interval.mk x iMax
--                          n =  MinusNode t (Leaf x)
--                          newStack = push (n, i') nextStack
--                          reducedNewStack = reduce newStack
--                      in aux reducedNewStack xs
--      | otherwise   = let i = PP.Interval.mk x x
--                          n =  Leaf x
--                          newStack = push (n, i) stack
--                      in aux newStack xs
--      where
--        iMin  = PP.Interval.getLeft i
--        iMax  = PP.Interval.getRight i
--
--    -- reduce top stack elements.
--    reduce :: [(SeparatingTree, PP.Interval.Interval)] -> [(SeparatingTree, PP.Interval.Interval)]
--    reduce [] = []
--    reduce [(i, t)] = [(i, t)]
--    reduce stack@((t, i) : (t', i') : nextStack)
--      | iMax+1 == iMin' = let i'' = PP.Interval.mk iMin iMax'
--                              n = PlusNode t t'
--                              newStack = push (n, i'') nextStack
--                          in reduce newStack
--      | iMax'+1 == iMin = let i'' = PP.Interval.mk  iMin' iMax
--                              n = MinusNode t t'
--                              newStack = push (n, i'') nextStack
--                          in reduce newStack
--      | otherwise       = stack
--      where
--        iMin  = PP.Interval.getLeft i
--        iMax  = PP.Interval.getRight i
--        iMin' = PP.Interval.getLeft i'
--        iMax' = PP.Interval.getRight i'
--
--    -- empty stack
--    emptyStack :: [(SeparatingTree, PP.Interval.Interval)]
--    emptyStack = []
--
--    -- push an element onto the stack
--    push :: (SeparatingTree, PP.Interval.Interval) -> [(SeparatingTree, PP.Interval.Interval)] -> [(SeparatingTree, PP.Interval.Interval)]
--    push x stack = x : stack
