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

  import qualified Data.List as L

  import qualified Data.Algorithm.PP.Perm         as PP.Perm

  (/+/) :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm
  p /+/ q = PP.Perm.mk (xs ++ ys)
    where
      xs = PP.Perm.toList p
      np  = L.length xs
      ys = L.map (+np) $ PP.Perm.toList q

  directSum :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm
  directSum = (/+/)

  (/-/) :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm
  p /-/ q = PP.Perm.mk (xs ++ ys)
    where
      nq = L.length ys
      xs = L.map (+nq) $ PP.Perm.toList p
      ys = PP.Perm.toList q

  skewSum :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm
  skewSum = (/+/)

  (/./) :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm
  p /./ q = L.map T.snd . L.sortBy cmpFst) . map proj . L.zip ixs . L.sortBy (cmpSnd) $ iys
    where
      cmpFst     = compare `on` T.fst
      cmpSnd     = compare `on` T.snd
      proj (x,y) = (T.fst y, T.snd x)
      ixs        = L.zip [1..] $ PP.Perm.toList p
      iys        = L.zip [1..] $ PP.Perm.toList q

  dot :: PP.Perm.Perm -> PP.Perm.Perm -> PP.Perm.Perm
  dot = (/./)
