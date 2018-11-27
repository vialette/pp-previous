module Data.Algorithm.PP.Perm.Small
(
  -- * Length 1 permutation
  p1

  -- * Length 2 permutations
, p12
, p21

  -- * Length 3 permutations
, p123
, p132
, p213
, p231
, p312
, p321

  -- * Length 4 permutations
, p1234
, p1243
, p1324
, p1342
, p1423
, p1432
, p2134
, p2143
, p2314
, p2341
, p2413
, p2431
, p3124
, p3142
, p3214
, p3241
, p3412
, p3421
, p4123
, p4132
, p4213
, p4231
, p4312
, p4321
)
where

  import qualified Data.Algorithm.PP.Perm as PP.Perm

  -- |'p1' returns the permutation 1.
  p1 :: PP.Perm.Perm
  p1 = PP.Perm.mk [1]

  -- |'p12' returns the permutation 12.
  p12 :: PP.Perm.Perm
  p12 = PP.Perm.mk [1,2]

  -- |'p21' returns the permutation 21.
  p21 :: PP.Perm.Perm
  p21 = PP.Perm.mk [2,1]

  -- |'p123' returns the permutation 123.
  p123 :: PP.Perm.Perm
  p123 = PP.Perm.mk [1,2,3]

  -- |'p132' returns the permutation 132.
  p132 :: PP.Perm.Perm
  p132 = PP.Perm.mk [1,3,2]

  -- |'p213' returns the permutation 213.
  p213 :: PP.Perm.Perm
  p213 = PP.Perm.mk [2,1,3]

  -- |'p231' returns the permutation 231.
  p231 :: PP.Perm.Perm
  p231 = PP.Perm.mk [2,3,1]

  -- |'p312' returns the permutation 312.
  p312 :: PP.Perm.Perm
  p312 = PP.Perm.mk [3,1,2]

  -- |'p321' returns the permutation 321.
  p321 :: PP.Perm.Perm
  p321 = PP.Perm.mk [3,2,1]

  -- |'p1234' returns the permutation 1234.
  p1234 :: PP.Perm.Perm
  p1234 = PP.Perm.mk [1,2,3,4]

  -- |'p1243' returns the permutation 1243.
  p1243 :: PP.Perm.Perm
  p1243 = PP.Perm.mk [1,2,4,3]

  -- |'p1324' returns the permutation 1324.
  p1324 :: PP.Perm.Perm
  p1324 = PP.Perm.mk [1,3,2,4]

  -- |'p1342' returns the permutation 1342.
  p1342 :: PP.Perm.Perm
  p1342 = PP.Perm.mk [1,3,4,2]

  -- |'p1423' returns the permutation 1423.
  p1423 :: PP.Perm.Perm
  p1423 = PP.Perm.mk [1,4,2,3]

  -- |'p1432' returns the permutation 1432.
  p1432 :: PP.Perm.Perm
  p1432 = PP.Perm.mk [1,4,3,2]

  -- |'p2134' returns the permutation 2134.
  p2134 :: PP.Perm.Perm
  p2134 = PP.Perm.mk [2,1,3,4]

  -- |'p2143' returns the permutation 2143.
  p2143 :: PP.Perm.Perm
  p2143 = PP.Perm.mk [2,1,4,3]

  -- |'p2314' returns the permutation 2314.
  p2314 :: PP.Perm.Perm
  p2314 = PP.Perm.mk [2,3,1,4]

  -- |'p2341' returns the permutation 2341.
  p2341 :: PP.Perm.Perm
  p2341 = PP.Perm.mk [2,3,4,1]

  -- |'p2413' returns the permutation 2413.
  p2413 :: PP.Perm.Perm
  p2413 = PP.Perm.mk [2,4,1,3]

  -- |'p2431' returns the permutation 2431.
  p2431 :: PP.Perm.Perm
  p2431 = PP.Perm.mk [2,4,3,1]

  -- |'p3124' returns the permutation 3124.
  p3124 :: PP.Perm.Perm
  p3124 = PP.Perm.mk [3,1,2,4]

  -- |'p3142' returns the permutation 3142.
  p3142 :: PP.Perm.Perm
  p3142 = PP.Perm.mk [3,1,4,2]

  -- |'p3214' returns the permutation 3214.
  p3214 :: PP.Perm.Perm
  p3214 = PP.Perm.mk [3,2,1,4]

  -- |'p3241' returns the permutation 3241.
  p3241 :: PP.Perm.Perm
  p3241 = PP.Perm.mk [3,2,4,1]

  -- |'p3412' returns the permutation 3412.
  p3412 :: PP.Perm.Perm
  p3412 = PP.Perm.mk [3,4,1,2]

  -- |'p3421' returns the permutation 3421.
  p3421 :: PP.Perm.Perm
  p3421 = PP.Perm.mk [3,4,2,1]

  -- |'p4123' returns the permutation 4123.
  p4123 :: PP.Perm.Perm
  p4123 = PP.Perm.mk [4,1,2,3]

  -- |'p4132' returns the permutation 4132.
  p4132 :: PP.Perm.Perm
  p4132 = PP.Perm.mk [4,1,3,2]

  -- |'p4213' returns the permutation 4213.
  p4213 :: PP.Perm.Perm
  p4213 = PP.Perm.mk [4,2,1,3]

  -- |'p4231' returns the permutation 4231.
  p4231 :: PP.Perm.Perm
  p4231 = PP.Perm.mk [4,2,3,1]

  -- |'p4312' returns the permutation 4312.
  p4312 :: PP.Perm.Perm
  p4312 = PP.Perm.mk [4,3,1,2]

  -- |'p4321' returns the permutation 4321.
  p4321 :: PP.Perm.Perm
  p4321 = PP.Perm.mk [4,3,2,1]
