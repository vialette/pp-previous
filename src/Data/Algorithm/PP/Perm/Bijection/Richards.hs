module Data.Algorithm.PP.Perm.Bijection.Richards
(
  richards
, invRichards
)
where

  import Control.Applicative
  import qualified Data.Foldable   as F
  import qualified Data.List       as L
  import qualified Data.Tuple      as T

  import qualified Data.Algorithm.PP.Dyck            as PP.Dyck
  import qualified Data.Algorithm.PP.Perm            as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List      as PP.Utils.List

  -- |'richards' 'perm'
  -- richards bijection from Dyck Path to 123-avoiding permutations.
  richards :: PP.Perm.Perm -> PP.Dyck.Path
  richards = PP.Dyck.mkUnsafe . aux . PP.Perm.getList
    where
      aux []  = []
      aux [_] = [PP.Dyck.UpStep, PP.Dyck.DownStep]
      aux xs  = [PP.Dyck.UpStep] ++ aux left ++ [PP.Dyck.DownStep] ++ aux right
        where
          maxY = F.maximum xs
          (left, right) = PP.Utils.List.splitOn maxY xs

  invRichards :: PP.Perm.Perm -> PP.Perm.Perm
  invRichards _ = PP.Perm.mkPerm [1]
