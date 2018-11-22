module Data.Algorithm.PP.Perm.Avoidance
(
  avoid
, contain
)
where

  import qualified Data.List      as L

  import qualified Data.Algorithm.PP.Perm         as PP.Perm

  -- 123 pattern.
  p123 :: PP.Perm.Perm
  p123 = PP.Perm.mk [1,2,3]

  -- 132' pattern.
  p132 :: PP.Perm.Perm
  p132 = PP.Perm.mk [1,3,2]

  p213 :: PP.Perm.Perm
  p213 = PP.Perm.mk [2,1,3]

  p231 :: PP.Perm.Perm
  p231 = PP.Perm.mk [2,3,1]

  p312 :: PP.Perm.Perm
  p312 = PP.Perm.mk [3,1,2]

  p321 :: PP.Perm.Perm
  p321 = PP.Perm.mk [3,2,1]

  p1234 :: PP.Perm.Perm
  p1234 = PP.Perm.mk [1,2,3,4]

  p1243 :: PP.Perm.Perm
  p1243 = PP.Perm.mk [1,2,4,3]

  p1324 :: PP.Perm.Perm
  p1324 = PP.Perm.mk [1,3,2,4]

  p1342 :: PP.Perm.Perm
  p1342 = PP.Perm.mk [1,3,4,2]

  p1423 :: PP.Perm.Perm
  p1423 = PP.Perm.mk [1,4,2,3]

  p1432 :: PP.Perm.Perm
  p1432 = PP.Perm.mk [1,4,3,2]

  p2134 :: PP.Perm.Perm
  p2134 = PP.Perm.mk [2,1,3,4]

  p2143 :: PP.Perm.Perm
  p2143 = PP.Perm.mk [2,1,4,3]

  p2314 :: PP.Perm.Perm
  p2314 = PP.Perm.mk [2,3,1,4]

  p2341 :: PP.Perm.Perm
  p2341 = PP.Perm.mk [2,3,4,1]

  p2413 :: PP.Perm.Perm
  p2413 = PP.Perm.mk [2,4,1,3]

  p2432 :: PP.Perm.Perm
  p2432 = PP.Perm.mk [2,4,3,1]

  p3124 :: PP.Perm.Perm
  p3124 = PP.Perm.mk [3,1,2,4]

  p3142 :: PP.Perm.Perm
  p3142 = PP.Perm.mk [3,1,4,2]

  p3214 :: PP.Perm.Perm
  p3214 = PP.Perm.mk [3,2,1,4]

  p3241 :: PP.Perm.Perm
  p3241 = PP.Perm.mk [3,2,4,1]

  p3412 :: PP.Perm.Perm
  p3412 = PP.Perm.mk [3,4,1,2]

  p3421 :: PP.Perm.Perm
  p3421 = PP.Perm.mk [3,4,2,1]

  p4123 :: PP.Perm.Perm
  p4123 = PP.Perm.mk [4,1,2,3]

  p4132 :: PP.Perm.Perm
  p4132 = PP.Perm.mk [4,1,3,2]

  p4213 :: PP.Perm.Perm
  p4213 = PP.Perm.mk [4,2,1,3]

  p4231 :: PP.Perm.Perm
  p4231 = PP.Perm.mk [4,2,3,1]

  p4312 :: PP.Perm.Perm
  p4312 = PP.Perm.mk [4,3,1,2]

  p4321 :: PP.Perm.Perm
  p4321 = PP.Perm.mk [4,3,2,1]

  contain :: [PP.Perm.Perm] -> PP.Perm.Perm -> Bool
  contain qs = not . avoid qs

  avoid :: [PP.Perm.Perm] -> PP.Perm.Perm -> Bool
  avoid qs = dispatchAvoid (L.sort qs)

  avoid213'231 p = go 1 (PP.Perm.len p) $ PP.Perm.toList p
    where
      go _ _ [] = True
      go minX maxX (x : xs)
        | x == minX = go (minX+1) maxX     xs
        | x == maxX = go minX     (maxX+1) xs
        | otherwise = False

  dispatchAvoid :: [PP.Perm.Perm] -> PP.Perm.Perm -> Bool
  dispatchAvoid [] _ = True
  dispatchAvoid qs p
    | qs == [p123] = True
    | qs == [p132] = True
    | qs == [p213] = True
    | qs == [p231] = True
    | qs == [p312] = True
    | qs == [p321] = True
    | qs == [p213, p231] = avoid213'231 p
    | qs == [p2413, p3142] = True
    | otherwise                  = False
