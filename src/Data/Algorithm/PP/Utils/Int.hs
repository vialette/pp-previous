module Data.Algorithm.PP.Utils.Int
(
  numDigits
)
where

  import qualified Data.List as L

  numDigits :: Int -> Int
  numDigits = L.length . show
  --numDigits :: (Integral a, Num b) => a -> b
  --numDigits n = fromIntegral (round (logBase 10 (fromIntegral n)) + 1)
