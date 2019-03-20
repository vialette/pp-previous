module Data.Algorithm.PP.Perm.Jump (
  -- * Numbers
    jumpNumber
  , jumpNumbers
  , distinctJumpNumbers

  -- * Sequences
  , jumpSequence
  , jumpSequences
  , jumpSequencesAssoc
  , distinctJumpSequences
  ) where

import Control.Arrow
import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T

import qualified Data.Algorithm.PP.Utils.List           as PP.Utils.List
import qualified Data.Algorithm.PP.Perm                 as PP.Perm
import qualified Data.Algorithm.PP.Perm.Generator.Basic as PP.Perm.Generator.Basic


{- | 'jumpSequence'

>>> mapM_ print . fmap (id &&& jumpSequence) $ perms 3
([1,2,3],[1,1])
([2,1,3],[1,2])
([3,2,1],[1,1])
([2,3,1],[1,2])
([3,1,2],[2,1])
([1,3,2],[2,1])
-}
jumpSequence :: PP.Perm.Perm -> [Int]
jumpSequence = L.map (abs . uncurry (-)) . PP.Utils.List.chunk2 . PP.Perm.getList

{- | 'jumpSequences'

>>> jumpSequences 3
[[1,1],[1,2],[1,1],[1,2],[2,1],[2,1]]
-}
jumpSequences :: Int -> [[Int]]
jumpSequences = L.map jumpSequence . PP.Perm.Generator.Basic.perms

jumpSequencesAssoc :: Int -> [([Int], [PP.Perm.Perm])]
jumpSequencesAssoc = L.map g . L.groupBy f . L.sort . L.map (jumpSequence &&& id) . PP.Perm.Generator.Basic.perms
  where
    f t t' = T.fst t == T.fst t'
    g      = (T.fst . L.head) &&& (L.map T.snd)


{- | 'distinctJumpSequences'

>>> distinctJumpSequences 3
[[1,1],[1,2],[2,1]]
-}
distinctJumpSequences :: Int -> [[Int]]
distinctJumpSequences = L.sort . PP.Utils.List.uniq . jumpSequences

{- | 'jumpNumber'

>>> mapM_ print . fmap (id &&& jumpNumber) $ perms 3
([1,2,3],2)
([2,1,3],3)
([3,2,1],2)
([2,3,1],3)
([3,1,2],3)
([1,3,2],3)
-}
jumpNumber :: PP.Perm.Perm -> Int
jumpNumber = F.sum . jumpSequence

{- | 'jumpNumbers'

>>> jumpNumbers 3
[2,3,2,3,3,3]
-}
jumpNumbers :: Int -> [Int]
jumpNumbers = L.map jumpNumber.  PP.Perm.Generator.Basic.perms

{- | 'distinctJumpNumbers'

>>> distinctJumpNumbers 3
[2,3]
-}
distinctJumpNumbers :: Int -> [Int]
distinctJumpNumbers =  L.sort . PP.Utils.List.uniq . jumpNumbers