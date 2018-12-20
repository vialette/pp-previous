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
  avoid_213_231 p = go 1 (PP.Perm.len p) $ PP.Perm.getList p
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
    | qs == [PP.Perm.Small.p_123] = True
    | qs == [PP.Perm.Small.p_132] = True
    | qs == [PP.Perm.Small.p_213] = True
    | qs == [PP.Perm.Small.p_231] = True
    | qs == [PP.Perm.Small.p_312] = True
    | qs == [PP.Perm.Small.p_321] = True
    | qs == [PP.Perm.Small.p_213, PP.Perm.Small.p_231] = avoid_213_231 p
    | qs == [PP.Perm.Small.p_2413, PP.Perm.Small.p_3142] = True
    | otherwise                  = False
