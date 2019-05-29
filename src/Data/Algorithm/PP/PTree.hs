module Data.Algorithm.PP.PTree
  (
    -- * Type
      PTree(..)

    -- * Constructing
    , mk

    -- *
    , labels
  ) where

import Data.Maybe
import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Set      as S
import qualified Data.Tuple    as T

import qualified Data.Algorithm.PP.Interval as PP.Interval
import qualified Data.Algorithm.PP.Perm     as PP.Perm

data PTree = PTree { getPerm     :: PP.Perm.Perm
                   , getInterval :: PP.Interval.Interval
                   , getPTrees   :: [PTree]
                   }

instance Show PTree where
  show pT = "(PTree " ++ p ++ " [" ++ pTs ++ "])"
    where
      p   = show $ getPerm pT
      pTs = L.intercalate "," . L.map show $ getPTrees pT

{- | 'mk' @p@ returns a decomposition tree of the permutation @p@ if it is super-separable.
-- Otherwise, the functions returns 'Nothing'.

-}
mk :: PP.Perm.Perm -> PTree
mk = go [] . L.map mkLeaf . PP.Perm.getList
  where
    mkLeaf :: Int -> PTree
    mkLeaf y = PTree { getPerm     = PP.Perm.mk [1]
                     , getInterval = PP.Interval.mkUnsafe y y
                     , getPTrees   = [] }

    go :: [PTree] -> [PTree] -> PTree
    go [pT] []      = pT
    go s    []      = fullReduce s
    go s (pT : pTs) = go s'' pTs
      where
        s'  = pT : s
        s'' = reduce s'

    consecutive :: [PTree] -> Bool
    consecutive = aux . L.sort . F.concatMap (PP.Interval.ints . getInterval)
      where
        aux ys = F.and $ L.zipWith ((==) . succ) ys (L.tail ys)

    split :: [PTree] -> ([PTree], [PTree])
    split = aux 0 []
      where
        aux _ pTs [] = ([], pTs)
        aux n pTs (pT : pTs')
          | n > 0 && consecutive (pT : pTs) = (pT : pTs, pTs')
          | otherwise                       = aux (n+1) (pT : pTs) pTs'

    reduce :: [PTree] -> [PTree]
    reduce []   = []
    reduce [pT] = [pT]
    reduce pTs  = case split pTs of ([], _)       -> pTs
                                    (pTs', pTs'') -> reduce $ (fullReduce pTs') : pTs''

    fullReduce :: [PTree] -> PTree
    fullReduce pTs = PTree { getPerm = p, getInterval = i, getPTrees = pTs }
      where
        is = L.map getInterval pTs
        p  = PP.Perm.mk $ L.map PP.Interval.getLeft is
        i  = fromJust $ PP.Interval.cover is

labels :: PTree -> [PP.Perm.Perm]
labels = L.sort . S.toList . aux S.empty
  where
    aux s pT = S.insert (getPerm pT) . F.foldl aux s $ getPTrees pT