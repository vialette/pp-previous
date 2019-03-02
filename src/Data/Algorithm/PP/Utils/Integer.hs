module Data.Algorithm.PP.Utils.Integer (
    -- * Encoding
    encode

    -- * Decoding
  , decode
  ) where

import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T

{- | 'encode' transforms a list of of distinct integers (@Int@) into a long integer (@Integer@).
-}
--encode :: (Foldable f) => f Int -> Integer
encode :: (Foldable t, Integral b, Num a) => t b -> a
encode = F.foldr (\ x acc -> 2^x + acc) 0

{- | 'decode' transforms an long integer (@Integer@) into a list of distinct integers (@Int@).
-}
--decode :: Integer -> [Int]
--decode = L.map (fromInteger . T.fst) . L.filter ((==) 1 . T.snd) . L.zip [0..] . L.unfoldr modDiv
decode = L.map T.fst . L.filter ((==) 1 . T.snd) . L.zip [0..] . L.unfoldr modDiv
  where
    modDiv 0 = Nothing
    modDiv n = let (q, r) = (n `divMod` 2) in Just (r, q)
