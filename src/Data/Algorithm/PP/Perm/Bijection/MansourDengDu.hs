module Data.Algorithm.PP.Perm.Bijection.MansourDengDu
(
  mansourDengDu
, invMansourDengDu
)
where

  import qualified Data.Algorithm.PP.Perm            as PP.Perm

  -- |'mansourDengDu' 'perm'
  mansourDengDu :: PP.Perm.Perm -> PP.Perm.Perm
  mansourDengDu _ = PP.Perm.mkPerm [1]

  -- |'invMansourDengDu' 'perm'
  invMansourDengDu :: PP.Perm.Perm -> PP.Perm.Perm
  invMansourDengDu _ = PP.Perm.mkPerm [1]
