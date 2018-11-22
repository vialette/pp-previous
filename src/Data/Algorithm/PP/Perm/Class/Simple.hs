module Data.Algorithm.PP.Perm.Class.Simple
(
  isSimple
)
where

  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Tuple    as T

  import qualified Data.Algorithm.PP.Perm       as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List

  -- |'isSimle' 'p' returns 'True' is the permutation 'p' is simple.
  isSimple :: PP.Perm.Perm -> Bool
  isSimple = F.all check . L.tail . F.concatMap (L.reverse . L.inits) . L.tails . PP.Perm.toList
    where
      check []  = True
      check [_] = True
      check xs  = any (T.uncurry (/=)) . L.zip [F.minimum xs..] $ L.sort xs
