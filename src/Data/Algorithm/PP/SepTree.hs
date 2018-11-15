module Data.Algorithm.PP.SepTree
(
  SepTree
)
where


  import qualified Data.Algorithm.PP.Perm as PP.Perm

  data SepTree = PNode SepTree SepTree
               | NNode SepTree SepTree
               | Leaf PP.Perm.T
