module Data.Algorithm.PP.Perm.Jump (
  -- * Numbers
    jumpNumber
  , jumpNumbers
  , distinctJumpNumbers

  -- * Sequences
  , jumpSequence
  , jumpSequences
  , distinctJumpSequences
  ) where

import qualified Data.Foldable as F
import qualified Data.List     as L

import qualified Data.Algorithm.PP.Utils.List           as PP.Utils.List
import qualified Data.Algorithm.PP.Perm                 as PP.Perm
import qualified Data.Algorithm.PP.Perm.Generator.Basic as PP.Perm.Generator.Basic


{- | 'jumpSequence'

>>> mapM_ print . fmap (\p -> (p, jumpSequence p)) $ perms 3
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
jumpSequences = L.map jumpSequence .  PP.Perm.Generator.Basic.perms

{- | 'jumpSequences'

>>> distinctJumpSequences 3
[[1,1],[1,2],[2,1]]
-}
distinctJumpSequences :: Int -> [[Int]]
distinctJumpSequences = L.sort . PP.Utils.List.uniq . jumpSequences


{- | 'jumpNumber'

>>> mapM_ print . fmap (\p -> (p, jumpNumber p)) $ perms 3
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

{- | 'jumpNumbers'

>>> distinctJumpNumbers 3
[2,3]
-}
distinctJumpNumbers :: Int -> [Int]
distinctJumpNumbers =  L.sort . PP.Utils.List.uniq . jumpNumbers