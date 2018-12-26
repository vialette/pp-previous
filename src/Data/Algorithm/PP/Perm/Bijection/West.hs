module Data.Algorithm.PP.Perm.Bijection.West
(
  west
, invWest
)
where

  import qualified Data.Algorithm.PP.Perm as PP.Perm

  -- | 'west' 'perm'
  west :: PP.Perm.Perm -> PP.Perm.Perm
  west _ = PP.Perm.mkPerm [1]

  -- | 'invWest' 'perm'
  invWest :: PP.Perm.Perm -> PP.Perm.Perm
  invWest _ = PP.Perm.mkPerm [1]
