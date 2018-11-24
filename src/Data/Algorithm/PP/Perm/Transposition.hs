module Data.Algorithm.PP.Perm.Transposition
(

)
where

  import qualified Control.Arrow as A
  import qualified Data.Foldable as F
  import qualified Data.List     as L

  import qualified Data.Algorithm.PP.Perm.Gen.Base as PP.Perm.Gen.Base
  import qualified Data.Algorithm.PP.Perm          as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List    as PP.Utils.List

  transpose :: Int -> Int -> PP.Perm.Perm -> PP.Perm.Perm
  transpose i j = PP.Perm.mk . PP.Utils.List.reversal i j k . PP.Perm.toList
