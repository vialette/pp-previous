{-|
Module      : Data.Algorithm.PP.Perm.Small
Description : Small standard permutations
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Convenient small permutations.
-}
module Data.Algorithm.PP.Perm.Small
  (
    -- * Length 1 permutation
    p_1

    -- * Length 2 permutations
  , p_12
  , p_21

    -- * Length 3 permutations
  , p_123
  , p_132
  , p_213
  , p_231
  , p_312
  , p_321

    -- * Length 4 permutations
  , p_1234
  , p_1243
  , p_1324
  , p_1342
  , p_1423
  , p_1432
  , p_2134
  , p_2143
  , p_2314
  , p_2341
  , p_2413
  , p_2431
  , p_3124
  , p_3142
  , p_3214
  , p_3241
  , p_3412
  , p_3421
  , p_4123
  , p_4132
  , p_4213
  , p_4231
  , p_4312
  , p_4321
  ) where

import qualified Data.Algorithm.PP.Perm as PP.Perm

-- |'p_1' returns the permutation 1.
p_1 :: PP.Perm.Perm
p_1 = PP.Perm.mkPerm [1]

-- |'p_12' returns the permutation 12.
p_12 :: PP.Perm.Perm
p_12 = PP.Perm.mkPerm [1,2]

-- |'p_21' returns the permutation 21.
p_21 :: PP.Perm.Perm
p_21 = PP.Perm.mkPerm [2,1]

-- |'p_123' returns the permutation 123.
p_123 :: PP.Perm.Perm
p_123 = PP.Perm.mkPerm [1,2,3]

-- |'p_132' returns the permutation 132.
p_132 :: PP.Perm.Perm
p_132 = PP.Perm.mkPerm [1,3,2]

-- |'p_213' returns the permutation 213.
p_213 :: PP.Perm.Perm
p_213 = PP.Perm.mkPerm [2,1,3]

-- |'p_231' returns the permutation 231.
p_231 :: PP.Perm.Perm
p_231 = PP.Perm.mkPerm [2,3,1]

-- |'p_312' returns the permutation 312.
p_312 :: PP.Perm.Perm
p_312 = PP.Perm.mkPerm [3,1,2]

-- |'p_321' returns the permutation 321.
p_321 :: PP.Perm.Perm
p_321 = PP.Perm.mkPerm [3,2,1]

-- |'p_1234' returns the permutation 1234.
p_1234 :: PP.Perm.Perm
p_1234 = PP.Perm.mkPerm [1,2,3,4]

-- |'p_1243' returns the permutation 1243.
p_1243 :: PP.Perm.Perm
p_1243 = PP.Perm.mkPerm [1,2,4,3]

-- |'p_1324' returns the permutation 1324.
p_1324 :: PP.Perm.Perm
p_1324 = PP.Perm.mkPerm [1,3,2,4]

-- |'p_1342' returns the permutation 1342.
p_1342 :: PP.Perm.Perm
p_1342 = PP.Perm.mkPerm [1,3,4,2]

-- |'p_1423' returns the permutation 1423.
p_1423 :: PP.Perm.Perm
p_1423 = PP.Perm.mkPerm [1,4,2,3]

-- |'p_1432' returns the permutation 1432.
p_1432 :: PP.Perm.Perm
p_1432 = PP.Perm.mkPerm [1,4,3,2]

-- |'p_2134' returns the permutation 2134.
p_2134 :: PP.Perm.Perm
p_2134 = PP.Perm.mkPerm [2,1,3,4]

-- |'p_2143' returns the permutation 2143.
p_2143 :: PP.Perm.Perm
p_2143 = PP.Perm.mkPerm [2,1,4,3]

-- |'p_2314' returns the permutation 2314.
p_2314 :: PP.Perm.Perm
p_2314 = PP.Perm.mkPerm [2,3,1,4]

-- |'p_2341' returns the permutation 2341.
p_2341 :: PP.Perm.Perm
p_2341 = PP.Perm.mkPerm [2,3,4,1]

-- |'p_2413' returns the permutation 2413.
p_2413 :: PP.Perm.Perm
p_2413 = PP.Perm.mkPerm [2,4,1,3]

-- |'p_2431' returns the permutation 2431.
p_2431 :: PP.Perm.Perm
p_2431 = PP.Perm.mkPerm [2,4,3,1]

-- |'p_3124' returns the permutation 3124.
p_3124 :: PP.Perm.Perm
p_3124 = PP.Perm.mkPerm [3,1,2,4]

-- |'p_3142' returns the permutation 3142.
p_3142 :: PP.Perm.Perm
p_3142 = PP.Perm.mkPerm [3,1,4,2]

-- |'p_3214' returns the permutation 3214.
p_3214 :: PP.Perm.Perm
p_3214 = PP.Perm.mkPerm [3,2,1,4]

-- |'p_3241' returns the permutation 3241.
p_3241 :: PP.Perm.Perm
p_3241 = PP.Perm.mkPerm [3,2,4,1]

-- |'p_3412' returns the permutation 3412.
p_3412 :: PP.Perm.Perm
p_3412 = PP.Perm.mkPerm [3,4,1,2]

-- |'p_3421' returns the permutation 3421.
p_3421 :: PP.Perm.Perm
p_3421 = PP.Perm.mkPerm [3,4,2,1]

-- |'p_4123' returns the permutation 4123.
p_4123 :: PP.Perm.Perm
p_4123 = PP.Perm.mkPerm [4,1,2,3]

-- |'p_4132' returns the permutation 4132.
p_4132 :: PP.Perm.Perm
p_4132 = PP.Perm.mkPerm [4,1,3,2]

-- |'p_4213' returns the permutation 4213.
p_4213 :: PP.Perm.Perm
p_4213 = PP.Perm.mkPerm [4,2,1,3]

-- |'p_4231' returns the permutation 4231.
p_4231 :: PP.Perm.Perm
p_4231 = PP.Perm.mkPerm [4,2,3,1]

-- |'p_4312' returns the permutation 4312.
p_4312 :: PP.Perm.Perm
p_4312 = PP.Perm.mkPerm [4,3,1,2]

-- |'p_4321' returns the permutation 4321.
p_4321 :: PP.Perm.Perm
p_4321 = PP.Perm.mkPerm [4,3,2,1]
