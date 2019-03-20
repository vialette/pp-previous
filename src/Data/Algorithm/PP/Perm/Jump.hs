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

import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List
import qualified Data.Algorithm.PP.Perm       as PP.Perm

{- | 'jumpSequence'
-}
jumpSequence :: PP.Perm.Perm -> [Int]
jumpSequence = L.map (abs . uncurry (-)) . PP.Utils.List.chunk2 . PP.Perm.getList

{- | 'jumpSequences'
-}
jumpSequences :: Int -> [[Int]]
jumpSequences = [jumpSequence p | p <- PP.Perm.perms n]

{- | 'jumpSequences'
-}
distinctJumpSequences :: Int -> [[Int]]
distinctJumpSequences = L.sort . PP.Utils.List.uniq . jumpSequencesæ


{- | 'jumpNumber'
-}
jumpNumber :: PP.Perm.Perm -> Int
jumpNumber = F.sum . jumpSequence

{- | 'jumpNumbers'
-}
jumpNumbers :: Int -> [Int]
jumpNumbers = [JumpNumber p | p <- PP.Perm.pems n]

{- | 'jumpNumbers'
-}
distinctJumpNumbers :: Int -> [Int]
distinctJumpNumbers =  L.sort . PP.Utils.List.uniq . jumpNumbers