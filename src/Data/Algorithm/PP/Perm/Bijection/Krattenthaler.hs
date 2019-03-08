module Data.Algorithm.PP.Perm.Bijection.Krattenthaler
(
  krattenthaler
, invKrattenthaler
)
where

  import qualified Data.Algorithm.PP.Perm            as PP.Perm

  -- |'krattenthaler' 'perm'
  krattenthaler :: PP.Perm.Perm -> PP.Perm.Perm
  krattenthaler _ = PP.Perm.mk [1]

  -- |'invKrattenthaler' 'perm'
  invKrattenthaler :: PP.Perm.Perm -> PP.Perm.Perm
  invKrattenthaler _ = PP.Perm.mk [1]
