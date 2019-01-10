module Data.Algorithm.PP.Utils.Integer
(
  -- * Encoding
  encode

  -- * Decoding
, decode
)
where

  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Tuple    as T

  -- |'encode' a list of distinct integers.
  encode :: (Foldable f) => f Int -> Integer
  encode = F.foldr (\ x acc -> 2^x + acc) 0

  -- |'decode' into a list of distinct integers.
  decode :: Integer -> [Int]
  decode = L.map (fromInteger . T.fst) . L.filter ((==) 1 . T.snd) . L.zip [0..] . L.unfoldr modDiv
    where
      modDiv 0 = Nothing
      modDiv n = let (q, r) = (n `divMod` 2) in Just (r, q)
