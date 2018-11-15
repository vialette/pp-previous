module Data.Algorithm.PP.Pattern
(
  Pattern

, orderIso
)
where

  newtype Pattern = Pattern { getElems :: [Int] }

  import qualified Data.Algorithm.PP.Perm as PP.Perm

  len = L.length . getElems

  orderIso :: Pattern -> Pattern -> Bool
  orderIso p p' = PP.Perm.fromList p == PP.Perm.fromList p' 
