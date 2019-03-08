{-|
Module      : Data.Algorithm.PP.Perm.Reversal
Description : Reversal in permutations
Copyright   : (c) Stéphane Vialette, 2018-2019
License     : GPL-3
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module Data.Algorithm.PP.Perm.Reversal (
  -- * Reversals
    reversal
  , reversal'
  , reversals
  , randomReversal

    -- * Prefix reversals
  , prefixReversal
  , prefixReversals
  , randomPrefixReversal

    -- * Suffix reversals
  , suffixReversal
  , suffixReversals
  , randomSuffixReversal
  ) where


import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T
import System.Random

import qualified Data.Algorithm.PP.Perm                   as PP.Perm
import qualified Data.Algorithm.PP.Perm.Bijection.Trivial as PP.Perm.Bijection.Trivial
import qualified Data.Algorithm.PP.Perm.Generator         as PP.Perm.Generator
import qualified Data.Algorithm.PP.Utils.List             as PP.Utils.List

-- |'reversal' 'i' 'j' 'p'
--
-- >>> let n = 3 in let p = identity n in mapM_ print [(i, j, reversal i j p) | i <- [0..n+1], j <- [i..n+1]]
-- (0,0,[1,2,3])
-- (0,1,[2,1,3])
-- (0,2,[3,2,1])
-- (0,3,[3,2,1])
-- (0,4,[3,2,1])
-- (1,1,[1,2,3])
-- (1,2,[2,1,3])
-- (1,3,[3,2,1])
-- (1,4,[3,2,1])
-- (2,2,[1,2,3])
-- (2,3,[1,3,2])
-- (2,4,[1,3,2])
-- (3,3,[1,2,3])
-- (3,4,[1,2,3])
-- (4,4,[1,2,3])
reversal :: Int -> Int -> PP.Perm.Perm -> PP.Perm.Perm
reversal i j = PP.Perm.mkPerm . PP.Utils.List.reversal i j . PP.Perm.getList

-- |'reversal' 'i' 'j' 'p'
--
-- >>> let n = 3 in let p = identity n in mapM_ print [(i, m, reversal' i m p) | i <- [0..n+1], m <- [i..n+1]]
-- (0,0,[1,2,3])
-- (0,1,[1,2,3])
-- (0,2,[2,1,3])
-- (0,3,[3,2,1])
-- (0,4,[3,2,1])
-- (1,1,[1,2,3])
-- (1,2,[2,1,3])
-- (1,3,[3,2,1])
-- (1,4,[3,2,1])
-- (2,2,[1,3,2])
-- (2,3,[1,3,2])
-- (2,4,[1,3,2])
-- (3,3,[1,2,3])
-- (3,4,[1,2,3])
-- (4,4,[1,2,3])
reversal' i j = PP.Perm.mkPerm . PP.Utils.List.reversal' i j . PP.Perm.getList

-- |'randomReversal' 'p' 'g'
--
-- >>> randomReversal (identity 8) (mkStdGen 123123)
-- ([1,2,3,4,5,8,7,6],1626896030 1655838864)
-- >>> randomReversal (identity 8) (mkStdGen 234234)
-- ([1,4,3,2,5,6,7,8],1859667740 1655838864)
randomReversal :: (RandomGen g) => PP.Perm.Perm -> g -> (PP.Perm.Perm, g)
randomReversal p g = (reversal i j p, g'')
  where
    (i, g')  = randomR (1, PP.Perm.len p) g
    (j, g'') = randomR (i+1, PP.Perm.len p) g'

-- |'reversals' 'p' returns all reversals of the permutation 'p'.
--
-- >>> reversals (identity 4)
-- [[2,1,3,4],[3,2,1,4],[4,3,2,1],[1,3,2,4],[1,4,3,2],[1,2,4,3]]
reversals :: PP.Perm.Perm -> [PP.Perm.Perm]
reversals p = [reversal i j p | i <- [1..PP.Perm.len p], j <- [i+1..PP.Perm.len p]]

-- |'prefixReversal' 'm' 'p'
--
-- >>> let n = 3 in let p = identity n in mapM_ print [(m, prefixReversal m p) | m <- [0..n+1]]
-- (0,[1,2,3])
-- (1,[1,2,3])
-- (2,[2,1,3])
-- (3,[3,2,1])
-- (4,[3,2,1])
prefixReversal :: Int -> PP.Perm.Perm -> PP.Perm.Perm
prefixReversal m = PP.Perm.mk . PP.Utils.List.prefixReversal m . PP.Perm.getList

-- |'randomPrefixReversal' 'p' 'g'
--
-- >>> randomPrefixReversal (identity 8) (mkStdGen 123123)
-- ([6,5,4,3,2,1,7,8],631716610 40692)
-- >>> randomPrefixReversal (identity 8) (mkStdGen 234234)
-- ([2,1,3,4,5,6,7,8],782745038 40692)
randomPrefixReversal :: (RandomGen g) => PP.Perm.Perm -> g -> (PP.Perm.Perm, g)
randomPrefixReversal p g = (prefixReversal i p, g')
  where
    (i, g')  = randomR (1, PP.Perm.len p) g

-- |'prefixReversals' 'p' returns all prefix reversals of the permutation 'p'.
--
-- >>> prefixReversals (identity 4)
-- [[1,2,3,4],[2,1,3,4],[3,2,1,4],[4,3,2,1]]
prefixReversals :: PP.Perm.Perm -> [PP.Perm.Perm]
prefixReversals p = [prefixReversal i p | i <- [1..PP.Perm.len p]]

-- |'suffixReversal' 'm' 'p'
--
-- >>> let n = 3 in let p = identity n in mapM_ print [(m, suffixReversal m p) | m <- [0..n+1]]
-- (0,[1,2,3])
-- (1,[1,2,3])
-- (2,[1,3,2])
-- (3,[3,2,1])
-- (4,[3,2,1])
suffixReversal :: Int -> PP.Perm.Perm -> PP.Perm.Perm
suffixReversal k = PP.Perm.Bijection.Trivial.rev . prefixReversal k . PP.Perm.Bijection.Trivial.rev

-- |'randomPrefixReversal' 'p' 'g'
--
-- >>> randomSuffixReversal (identity 8) (mkStdGen 123123)
-- ([1,2,8,7,6,5,4,3],631716610 40692)
-- >>> randomSuffixReversal (identity 8) (mkStdGen 234234)
-- ([1,2,3,4,5,6,8,7],782745038 40692)
randomSuffixReversal :: (RandomGen g) => PP.Perm.Perm -> g -> (PP.Perm.Perm, g)
randomSuffixReversal p g = (suffixReversal i p, g')
  where
    (i, g')  = randomR (1, PP.Perm.len p) g

-- |'suffixReversals' 'p' returns all suffix reversals of the permutation 'p'.
--
-- >>> suffixReversals (identity 4)
-- [[1,2,3,4],[1,2,4,3],[1,4,3,2],[4,3,2,1]]
suffixReversals :: PP.Perm.Perm -> [PP.Perm.Perm]
suffixReversals p = [suffixReversal i p | i <- [1..PP.Perm.len p]]
