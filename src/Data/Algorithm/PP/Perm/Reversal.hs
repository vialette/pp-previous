module Data.Algorithm.PP.Perm.Reversal (
    reversal
  , reversal'
  , prefixReversal
  , suffixReversal
  ) where


  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Tuple    as T

  import qualified Data.Algorithm.PP.Perm.Generator as PP.Perm.Generator
  import qualified Data.Algorithm.PP.Perm           as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List     as PP.Utils.List

  -- |'reversal' 'i' 'j' 'p'
  --
--
  reversal :: Int -> Int -> PP.Perm.Perm -> PP.Perm.Perm
  reversal i j = PP.Perm.mkPerm . PP.Utils.List.reversal i j . PP.Perm.getList

  -- |'reversal' 'i' 'j' 'p'
  reversal' :: Int -> Int -> PP.Perm.Perm -> PP.Perm.Perm
  reversal' i j = PP.Perm.mkPerm . PP.Utils.List.reversal' i j . PP.Perm.getList

  -- |'prefixReversal' 'm' 'p'
  prefixReversal :: Int -> PP.Perm.Perm -> PP.Perm.Perm
  prefixReversal m = PP.Perm.mkPerm . PP.Utils.List.prefixReversal m . PP.Perm.getList


  -- |'suffixReversal' 'm' 'p'
  suffixReversal :: Int -> PP.Perm.Perm -> PP.Perm.Perm
  suffixReversal = reversal 0
