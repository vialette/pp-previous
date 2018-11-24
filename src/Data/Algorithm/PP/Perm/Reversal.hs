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

  -- |'reversal' 'i' 'j' 'p'
  reversal :: Int -> Int -> PP.Perm.Perm -> PP.Perm.Perm
  reversal i j = PP.Perm.mk . PP.Utils.List.reversal i j . PP.Perm.toList

  -- |'prefixReversal' 'm' 'p'
  prefixReversal :: Int -> PP.Perm.Perm -> PP.Perm.Perm
  prefixReversal = reversal 0

  -- |'dReversals' 'p' returns the list of permutations 'q1', 'q2', ... 'qk'
  -- with 'p = q1'
  --
  -- >>> dReversal (mk [4,2,6,4,3,1])
  -- [4,2,6,5,3,1],[5,6,2,4,3,1],[3,4,2,6,5,1],[2,4,3,6,5,1],[4,2,3,6,5,1],[6,3,2,4,5,1],[1,5,4,2,3,6]]
  dReversal :: PP.Perm.Perm -> [PP.Perm.Perm]
  dReversal = L.map PP.Perm.mk . L.reverse . aux [] . PP.Perm.toList
    where
      aux acc []         = acc
      aux acc (1 : xs)   = (1 : xs) : acc
      aux acc xs@(i : _) = aux (xs : acc) (PP.Utils.List.prefixReversal i xs)

  -- 'extremalDReversals' 'n'
  --
  -- >>> extremalDReversals 4
  -- (4,[[3,1,4,2],[2,4,1,3]])
  -- >>> dReversal (mk [3,1,4,2])
  -- [[3,1,4,2],[4,1,3,2],[2,3,1,4],[3,2,1,4],[1,2,3,4]]
  -- dReversal (mk [2,4,1,4])
  -- [[2,3,1,4],[3,2,1,4],[1,2,3,4]]
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
