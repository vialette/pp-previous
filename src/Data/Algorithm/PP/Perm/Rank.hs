module Data.Algorithm.PP.Perm.Rank
(
  rank1
, unrank1

, rank2
, unrank2
)
where

  import qualified Data.Algorithm.PP.Perm       as PP.Perm

  rank1 :: PP.Perm.Perm -> Int
  rank1 p = 0

  unrank1 :: Int -> Int -> PP.Perm.Perm
  unrank1 _ _ = PP.Perm.identity 5


  rank2 :: PP.Perm.Perm -> Int
  rank2 p = 0

  unrank2 :: Int -> Int -> PP.Perm.Perm
  unrank2 _ _ = PP.Perm.identity 5
