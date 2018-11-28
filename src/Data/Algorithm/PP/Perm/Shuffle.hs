module Data.Algorithm.PP.Perm.Shuffle
(
  -- * Generating
  shuffle
, shuffle2
, shuffle3
, shuffle4

  -- * Testing
, shuffleOf2
, shuffleOf2'
, shuffleOf2''
)
where

  import qualified Control.Arrow as A
  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Tuple    as T

  import qualified Data.Algorithm.PP.Combi      as PP.Combi
  import qualified Data.Algorithm.PP.Perm       as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List

  -- |'shuffleOf2' 'p' 'q' 'r' returns 'True' if the permutation 'r' is in the
  -- shuffle of permutations 'p' and 'q'.
  --
  -- >>> shuffleOf2 (mk [1,3,2]) (mk [3,1,2]) (mk [1,5,3,6,2,4])
  -- True
  shuffleOf2 :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm -> Bool
  shuffleOf2 p q = not . L.null . shuffleOf2' p q

  -- |'shuffleOf2'' 'p' 'q' 'r'
  --
  -- >>> shuffleOf2' (mk [1,3,2]) (mk [3,1,2]) (mk [1,5,3,6,2,4])
  -- [([1,5,3],[6,2,4]),([1,6,2],[5,3,4])]
  -- >>> mk [1,3,2] == mk [1,5,3] && mk [3,1,2] == mk [6,2,4] -- check first solution
  -- True
  -- >>> mk [1,3,2] == mk [1,6,2] && mk [3,1,2] == mk [5,3,4] -- check second solution
  -- True
  shuffleOf2' :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm -> [(PP.Perm.Pattern, PP.Perm.Pattern)]
  shuffleOf2' p q r = L.filter (\(p', q') ->  p == p' && q == q') $ PP.Perm.partitions r (PP.Perm.len p) (PP.Perm.len q)

  -- |'shuffleOf2''' 'p' 'q' 'r'
  --
  -- >>> shuffleOf2'' (mk [1,3,2]) (mk [3,1,2]) (mk [1,5,3,6,2,4])
  -- Just ([1,5,3],[6,2,4])
  shuffleOf2'' :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm -> Maybe (PP.Perm.Pattern, PP.Perm.Pattern)
  shuffleOf2'' p q = PP.Utils.List.safeHead . shuffleOf2' p q


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
