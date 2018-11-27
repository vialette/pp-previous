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

  import qualified Data.Algorithm.PP.Perm         as PP.Perm

  -- |'p' '/+/' 'q'
  (/+/) :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm
  p /+/ q = PP.Perm.mk (xs ++ ys)
    where
      xs = PP.Perm.toList p
      np  = L.length xs
      ys = L.map (+np) $ PP.Perm.toList q

  -- | Alias for '/+/'.
  directSum :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm
  directSum = (/+/)

  -- |'p' '/-/' 'q'
  (/-/) :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm
  p /-/ q = PP.Perm.mk (xs ++ ys)
    where
      nq = L.length ys
      xs = L.map (+nq) $ PP.Perm.toList p
      ys = PP.Perm.toList q

  -- | Alias for '/-/'.
  skewSum :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm
  skewSum = (/+/)

  -- |'p' '/./' 'q'
  (/./) :: PP.Perm.Perm -> PP.Perm.Perm -> Maybe PP.Perm.Perm
  p /./ q
    | np == nq  = Just . PP.Perm.mk . L.map T.snd . L.sortBy cmpFst . L.zipWith (T.curry proj) ips . L.sortBy cmpSnd $ iqs
    | otherwise = Nothing
    where
      np         = PP.Perm.len p
      nq         = PP.Perm.len q
      cmpFst     = compare `on` T.fst
      cmpSnd     = compare `on` T.snd
      proj (x,y) = (T.fst y, T.snd x)
      ips        = L.zip [1..] $ PP.Perm.toList p
      iqs        = L.zip [1..] $ PP.Perm.toList q

  -- | Alias for '/./'.
  dot :: PP.Perm.Perm -> PP.Perm.Perm -> Maybe PP.Perm.Perm
  dot = (/./)
