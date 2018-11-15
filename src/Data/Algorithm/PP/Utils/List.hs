module Data.Algorithm.PP.Utils.List
(
  uniq
, chunk
, chunk2
, chunk3
)
where

  import qualified Data.List as L
  import qualified Data.Set as S

  -- 'uniq xs' removes duplicates in 'xs'.
  uniq :: Ord a => [a] -> [a]
  uniq = S.toList . S.fromList

  chunk :: Int -> [a] -> [[a]]
  chunk n = L.takeWhile ((== n) . L.length) . L.transpose . L.take n . L.iterate L.tail

  chunk2 :: [a] -> [[a]]
  chunk2 = chunk 2

  chunk3 :: [a] -> [[a]]
  chunk3 = chunk 3
