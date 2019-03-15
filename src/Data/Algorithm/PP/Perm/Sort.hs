{-|
Module      : Data.Algorithm.PP.Perm.Sort
Description : Sorting permutations
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Sorting permutations.
-}
module Data.Algorithm.PP.Perm.Sort
  (
    stackSort
  , isStackSortable
  )  where

import qualified Data.List as L

import qualified Data.Algorithm.PP.Perm          as PP.Perm
import qualified Data.Algorithm.PP.Perm.Property as PP.Perm.Property

{- | 'stackSort' @p@ applies the following algorithm to the permutation @p@:

* Initialize an empty stack

* For each input value x:

  * While the stack is nonempty and x is larger than the top item on the stack, pop the stack to the output

  * Push x onto the stack

* While the stack is nonempty, pop it to the output

The algorithm correctly sorts the permutation is it avoids 231
(<https://en.wikipedia.org/wiki/Stack-sortable_permutation>)

>>> mapM_ putStrLn . fmap (\ p -> "stackSort(" ++ show p ++ ") = " ++ show (stackSort p)) $ perms 3
stackSort([1,2,3]) = [1,2,3]
stackSort([2,1,3]) = [1,2,3]
stackSort([3,2,1]) = [1,2,3]
stackSort([2,3,1]) = [2,1,3]
stackSort([3,1,2]) = [1,2,3]
stackSort([1,3,2]) = [1,2,3]
-}
stackSort :: PP.Perm.Perm -> PP.Perm.Perm
stackSort = PP.Perm.mk . aux [] [] . PP.Perm.getList
  where
    aux acc []           []       = L.reverse acc
    aux acc (s : ss)     []       = aux (s : acc) ss []
    aux acc []           (x : xs) = aux acc [x] xs
    aux acc ss'@(s : ss) xs'@(x : xs)
      | x > s     = aux (s : acc) ss        xs'
      | otherwise = aux acc       (x : ss') xs

{- | 'stackSortable' @p@ returns @True@ if the permutation @p@ is stack sortable
(i.e. if it avoids 231 (<https://en.wikipedia.org/wiki/Stack-sortable_permutation>)).

>>> mapM_ putStrLn . fmap (\ p -> "stackSort(" ++ show p ++ ") = " ++ show (stackSortable p)) $ perms 3
stackSort([1,2,3]) = True
stackSort([2,1,3]) = True
stackSort([3,2,1]) = True
stackSort([2,3,1]) = False
stackSort([3,1,2]) = True
stackSort([1,3,2]) = True
-}
isStackSortable :: PP.Perm.Perm -> Bool
isStackSortable = PP.Perm.Property.sorted . stackSort

dequeSort :: PP.Perm.Perm -> PP.Perm.Perm
dequeSort p = p

isDequeSortable :: PP.Perm.Perm -> Bool
isDequeSortable = PP.Perm.Property.sorted . dequeSort

parallelQueues2Sort :: PP.Perm.Perm -> PP.Perm.Perm
parallelQueues2Sort p = p

isParallelQueues2Sortable :: PP.Perm.Perm -> Bool
isParallelQueues2Sortable = PP.Perm.Property.sorted . parallelQueues2Sort
