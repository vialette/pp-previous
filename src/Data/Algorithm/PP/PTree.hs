module Data.Algorithm.PP.PTree
  (
    -- * Type
      PTree(..)

    -- * Constructing
    , mk

    -- * Querying
    , height
    , labels

    -- * Displaying
    , indent
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

{- | 'height' @pT@ returns the height of the PTree @pT@.

>>> pT = PTree.mk $ Perm.mk [1,7,6,5,2,8,3]
>>> pT
(PTree [1,2] [(PTree [1] []),(PTree [3,1,4,2] [(PTree [2,1] [(PTree [2,1] [(PTree [1] []),(PTree [1] [])]),(PTree [1] [])]),(PTree [1] []),(PTree [1] []),(PTree [1] [])])])
>>> PTree.height pT
4
-}
height :: PTree -> Int
height pT
  | L.null pTs = 0
  | otherwise  = succ . F.maximum $ L.map height pTs
  where
    pTs = getPTrees pT

labels :: PTree -> [PP.Perm.Perm]
labels = L.sort . S.toList . aux S.empty
  where
    aux s pT = S.insert (getPerm pT) . F.foldl aux s $ getPTrees pT

{- | 'indent' @pT@ returns an indented string representation of the PTree @pT@.

>>> putStr . PTree.indent . PTree.mk $ P.mk [1,7,6,5,2,8,9,3]
[1,2] {
  1
  [3,1,4,2] {
    [2,1] {
      [2,1] {
        6
        5
      }
      4
    }
    2
    [1,2] {
      7
      8
    }
    3
  }
}
-}
indent :: PTree -> String
indent = aux 0
  where
    aux o pT
      | PP.Interval.len i == 0 = offset                       ++
                                 show (PP.Interval.getLeft i) ++
                                 "\n"

      | otherwise              = offset                       ++
                                 show p                       ++
                                 " {\n"                       ++
                                 F.concatMap (aux (o+2)) pTs  ++
                                 offset                       ++
                                 "}\n"
      where
        offset = L.take o $ L.repeat ' '
        p      = getPerm     pT
        i      = getInterval pT
        pTs    = getPTrees   pT
