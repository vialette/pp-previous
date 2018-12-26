module Data.Algorithm.PP.Perm.Bijection.Reifegerste
(
  reifegerste
, invReifegerste
)
where

  import qualified Data.Algorithm.PP.Perm            as PP.Perm

  -- |'reifegerste' 'perm'
  reifegerste :: PP.Perm.Perm -> PP.Perm.Perm
  reifegerste _ = PP.Perm.mkPerm [1]

  -- |'invReifegerste' 'perm'
  invReifegerste :: PP.Perm.Perm -> PP.Perm.Perm
  invReifegerste _ = PP.Perm.mkPerm [1]
