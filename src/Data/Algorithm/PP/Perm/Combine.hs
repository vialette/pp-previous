module Data.Algorithm.PP.Perm.Combine
(
  (/+/)
, directSum

, (/-/)
, skewSum

, (/./)
, dot
)
where

  import qualified Data.List  as L
  import qualified Data.Tuple as T
  import Data.Function (on)

  import qualified Data.Algorithm.PP.Utils.Maybe as PP.Utils.Maybe
  import qualified Data.Algorithm.PP.Perm        as PP.Perm

  -- |'p' '/+/' 'q'
  (/+/) :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm
  p /+/ q = PP.Perm.mkPerm (xs ++ ys)
    where
      xs = PP.Perm.getList p
      np  = L.length xs
      ys = L.map (+np) $ PP.Perm.getList q

  -- | Alias for '/+/'.
  directSum :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm
  directSum = (/+/)

  -- |'p' '/-/' 'q'
  (/-/) :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm
  p /-/ q = PP.Perm.mkPerm (xs ++ ys)
    where
      nq = L.length ys
      xs = L.map (+nq) $ PP.Perm.getList p
      ys = PP.Perm.getList q

  -- | Alias for '/-/'.
  skewSum :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm
  skewSum = (/+/)

  -- |'p' '/./' 'q'
  (/./) :: PP.Perm.Perm -> PP.Perm.Perm -> Maybe PP.Perm.Perm
  p /./ q = PP.Utils.Maybe.whenMaybe (PP.Perm.len p == PP.Perm.len q)
            (PP.Perm.mkPerm . L.map T.snd . L.sortBy cmpFst . L.zipWith (T.curry proj) ips $ L.sortBy cmpSnd iqs)
    where
      cmpFst     = compare `on` T.fst
      cmpSnd     = compare `on` T.snd
      proj (x,y) = (T.fst y, T.snd x)
      ips        = L.zip [1..] $ PP.Perm.getList p
      iqs        = L.zip [1..] $ PP.Perm.getList q

  -- | Alias for '/./'.
  dot :: PP.Perm.Perm -> PP.Perm.Perm -> Maybe PP.Perm.Perm
  dot = (/./)
