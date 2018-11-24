module Data.Algorithm.PP.Perm.Reversal
(
  reversal
, prefixReversal

, dReversal
, extremalDReversals
)
where

  import qualified Control.Arrow as A
  import qualified Data.Foldable as F
  import qualified Data.List     as L

  import qualified Data.Algorithm.PP.Perm.Gen.Base as PP.Perm.Gen.Base
  import qualified Data.Algorithm.PP.Perm          as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List    as PP.Utils.List

  reversal :: Int -> Int -> PP.Perm.Perm -> PP.Perm.Perm
  reversal i j = PP.Perm.mk . PP.Utils.List.reversal i j . PP.Perm.toList

  prefixReversal :: Int -> PP.Perm.Perm -> PP.Perm.Perm
  prefixReversal = reversal 0

  dReversal :: PP.Perm.Perm -> [PP.Perm.Perm]
  dReversal = L.map PP.Perm.mk . L.reverse . aux [] . PP.Perm.toList
    where
      aux acc []         = acc
      aux acc (1 : xs)   = (1 : xs) : acc
      aux acc xs@(i : _) = aux (xs : acc) (PP.Utils.List.prefixReversal i xs)

  extremalDReversals :: Int -> (Int, [PP.Perm.Perm])
  extremalDReversals = aux 0 [] . L.map dReversal . PP.Perm.Gen.Base.derangements
    where
      aux m acc []  = (m-1, acc)
      aux m acc (ps : pss)
        | m == m'   = aux m  (L.head ps : acc) pss
        | m >  m'   = aux m  acc               pss
        | otherwise = aux m' [L.head ps]       pss
          where
            m' = L.length ps
