module Data.Algorithm.PP.Perm.Bijection.Knuth
(
  knuth
, standard
, invKnuth
)
where

  import Control.Applicative
  import qualified Data.Foldable   as F
  import qualified Data.List       as L
  import qualified Data.Tuple      as T

  import qualified Data.Algorithm.PP.Dyck            as PP.Dyck
  import qualified Data.Algorithm.PP.Perm            as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List      as PP.Utils.List

  -- |Alias for 'knuth'.
  standard :: PP.Perm.Perm -> PP.Dyck.LPath ()
  standard = knuth

  -- 'knuth' 'perm'
  -- bijection from
  -- 132-avoiding permutations to Dyck paths.
  --
  -- >>> knuth (mkPerm [7,5,6,4,2,1,3])
  -- ()(())()(()())
  knuth :: PP.Perm.Perm -> PP.Dyck.LPath ()
  knuth = PP.Dyck.mk . aux . PP.Perm.getList
    where
      aux []  = []
      aux [_] = [PP.Dyck.LUp (), PP.Dyck.LDown ()]
      aux xs  = [PP.Dyck.LUp ()] ++ aux left ++ [PP.Dyck.LDown ()] ++ aux right
        where
          maxY = F.maximum xs
          (left, right) = PP.Utils.List.splitOn maxY xs

  invKnuth :: PP.Perm.Perm -> PP.Perm.Perm
  invKnuth _ = PP.Perm.mkPerm [1]
