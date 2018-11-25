module Data.Algorithm.PP.Perm.Shuffle
(
  shuffle
, shuffle2
, shuffle3
, shuffle4
)
where

  import qualified Data.List      as L

  import qualified Data.Algorithm.PP.Perm       as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List


  -- |'shuffle2' 'p' 'q' return all distinct permutations that can be be obtained by
  -- shuffling permutation 'p' and 'q'.
  --
  -- >>>  shuffle2 (mk [1,2]) (mk [2,1])
  -- [[1,3,4,2],[1,3,4,2],[1,3,2,4],[3,1,4,2],[3,1,2,4],[3,1,2,4]]
  shuffle2 :: PP.Perm.Perm -> PP.Perm.Perm -> [PP.Perm.Perm]
  shuffle2 p q = shuffle [p, q]

  -- |'shuffle3' 'p' 'q' 'r' return all distinct permutations that can be be obtained by
  -- shuffling permutation 'p', 'q' and 'r'.
  shuffle3 :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm -> [PP.Perm.Perm]
  shuffle3 p q r = shuffle [p, q, r]

  -- |'shuffle4' 'p' 'q' 'r' 's' return all distinct permutations that can be be obtained by
  -- shuffling permutation 'p', 'q', 'r' and 's'.
  shuffle4 :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm -> [PP.Perm.Perm]
  shuffle4 p q r s = shuffle [p, q, r, s]

  -- |'shuffle' 'ps' return all distinct permutations that can be be obtained by
  -- shuffling permutations in 'ps'.
  --
  -- >>> shuffle [mk [1,2], mk [2,1]]
  -- [[1,3,4,2],[1,3,4,2],[1,3,2,4],[3,1,4,2],[3,1,2,4],[3,1,2,4]]
  shuffle :: [PP.Perm.Perm] -> [PP.Perm.Perm]
  shuffle = L.map PP.Perm.mk . PP.Utils.List.shuffle . L.map PP.Perm.toList
