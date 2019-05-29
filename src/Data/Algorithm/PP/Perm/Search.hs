module Data.Algorithm.PP.Perm.Search
  (
    -- * Query
      search
  )
  where

import qualified Data.Foldable as F
import qualified Data.List     as L

import qualified Data.Algorithm.PP.Perm         as PP.Perm
import qualified Data.Algorithm.PP.Perm.Pattern as PP.Perm.Pattern


{- | 'search' @q@ @p@ returns true iff permutation @p@ occurs in permutation @p@ as a pattern.

-}
search :: PP.Perm.Perm -> PP.Perm.Perm -> Bool
search q = F.any ((==) q) . PP.Perm.Pattern.kPatterns k
  where
    k = PP.Perm.len q
