module Data.Algorithm.PP.Perm.Bijection.KnuthRichards
(
  knuthRichards
, invKnuthRichards
)
where

  import Control.Applicative
  import qualified Data.Foldable   as F
  import qualified Data.List       as L
  import qualified Data.Tuple      as T

  import qualified Data.Algorithm.PP.Dyck            as PP.Dyck
  import qualified Data.Algorithm.PP.Perm            as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List      as PP.Utils.List


  -- |'knuthRichards' 'perm'
  knuthRichards :: PP.Perm.Perm -> PP.Dyck.LPath ()
  knuthRichards _ = PP.Perm.mkPerm [] -- PP.Perm.Bijection.Richards.richards . PP.Perm.Bijection.Knuth.knuth

  -- |'invKnuthRichards' 'perm'
  invKnuthRichards :: PP.Perm.Perm -> PP.Dyck.LPath ()
  invKnuthRichards _ = PP.Perm.mkPerm []
