module Data.Algorithm.PP.Perm.Avoidance
(
  avoid
, contain
)
where

  import qualified Data.List as L

  import qualified Data.Algorithm.PP.Perm       as PP.Perm
  import qualified Data.Algorithm.PP.Perm.Small as PP.Perm.Small

  -- |'contain' 'ps' 'q' returns True if the permutation 'q' contains every
  -- permutations in 'qs' as a pattern.
  contain :: [PP.Perm.Perm] -> PP.Perm.Perm -> Bool
  contain qs = not . avoid qs

  -- |'avoid' 'ps' 'q'
  avoid :: [PP.Perm.Perm] -> PP.Perm.Perm -> Bool
  avoid qs = dispatchAvoid (L.sort qs)

  -- |'avoid213'231'
  avoid213'231 p = go 1 (PP.Perm.len p) $ PP.Perm.toList p
    where
      go _ _ [] = True
      go minX maxX (x : xs)
        | x == minX = go (minX+1) maxX     xs
        | x == maxX = go minX     (maxX+1) xs
        | otherwise = False

  --
  dispatchAvoid :: [PP.Perm.Perm] -> PP.Perm.Perm -> Bool
  dispatchAvoid [] _ = True
  dispatchAvoid qs p
    | qs == [PP.Perm.Small.p123] = True
    | qs == [PP.Perm.Small.p132] = True
    | qs == [PP.Perm.Small.p213] = True
    | qs == [PP.Perm.Small.p231] = True
    | qs == [PP.Perm.Small.p312] = True
    | qs == [PP.Perm.Small.p321] = True
    | qs == [PP.Perm.Small.p213, PP.Perm.Small.p231] = avoid213'231 p
    | qs == [PP.Perm.Small.p2413, PP.Perm.Small.p3142] = True
    | otherwise                  = False
