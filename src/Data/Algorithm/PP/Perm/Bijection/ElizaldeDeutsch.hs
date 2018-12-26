module Data.Algorithm.PP.Perm.Bijection.ElizaldeDeutsch
(
  elizaldeDeutsch
, invElizaldeDeutsch
)
where

  import qualified Data.Algorithm.PP.Perm            as PP.Perm

  -- |'elizaldeDeutsch' 'perm'
  elizaldeDeutsch :: PP.Perm.Perm -> PP.Perm.Perm
  elizaldeDeutsch _ = PP.Perm.mkPerm []

  -- |'invElizaldeDeutsch' 'perm'
  invElizaldeDeutsch :: PP.Perm.Perm -> PP.Perm.Perm
  invElizaldeDeutsch _ = PP.Perm.mkPerm []
