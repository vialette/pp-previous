module Data.Algorithm.PP.Perm.Features
(

  -- *
  leftmost
, rightmost
)
where

  import qualified Data.Algorithm.PP.Perm as PP.Perm

  -- |'leftmost' 'perm' returns the first (i.e. leftmost) element of the
  -- permutation 'perm'.
  leftmost :: PP.Perm.Perm -> Int
  leftmost = L.head . PP.Perm.getList

  -- |'rightmost' 'perm' returns the last (i.e. rightmost) element of the
  -- permutation 'perm'.
  rightmost :: PP.Perm.Perm -> Int
  rightmost = L.last . PP.Perm.getList
