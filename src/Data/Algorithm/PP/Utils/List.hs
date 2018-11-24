module Data.Algorithm.PP.Utils.List
(
  safeHead
, safeTail

, factor
, factor'

, uniq
, chunk
, chunk2
, chunk3

, shuffle
, perfectShuffle

, reversal
, reversal'
, prefixReversal

, transpose
)
where

  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Set      as S

  safeHead :: [a] -> Maybe a
  safeHead [] = Nothing
  safeHead (x:xs) = Just x

  safeTail :: [a] -> Maybe [a]
  safeTail [] = Nothing
  safeTail (x:xs) = Just xs

  factor :: Int -> Int -> [a] -> [a]
  factor i j = L.take (j-i+1) . L.drop i

  factor' :: Int -> Int -> [a] -> [a]
  factor' i k = factor i (i+k-1)

  -- |'uniq' xs removes all duplicates in 'xs'. No order is guaranteed.
  --
  -- >>> uniq "abcacbb"
  -- "abc"
  uniq :: Ord a => [a] -> [a]
  uniq = S.toList . S.fromList

  chunk :: Int -> [a] -> [[a]]
  chunk n = L.takeWhile ((== n) . L.length) . L.transpose . L.take n . L.iterate L.tail

  chunk2 :: [a] -> [[a]]
  chunk2 = chunk 2

  chunk3 :: [a] -> [[a]]
  chunk3 = chunk 3

  shuffle2 :: [a] -> [a] -> [[a]]
  shuffle2 []       []       = [[]]
  shuffle2 []       ys       = [ys]
  shuffle2 xs       []       = [xs]
  shuffle2 (x : xs) (y : ys) = L.map (x :) withY ++ L.map (y :) withX
    where
      withX = shuffle2 (x : xs) ys
      withY = shuffle2 xs       (y : ys)

  shuffle :: [[a]] -> [[a]]
  shuffle []               = [[]]
  shuffle [xs]             = [xs]
  shuffle (xs : xs' : xss) = F.concat [ shuffle2 ys ys' |
                                        ys  <- shuffle2 xs xs'
                                      , ys' <- shuffle xss]

  -- |'perfectShuffle' 'xs' 'ys' perfectly interleaves the lists 'xs' and 'ys',
  -- stating with 'xs'.
  --
  -- >>> perfectShuffle [1,2,3,4] [5,6,7,8]
  -- [1,5,2,6,3,7,4,8]
  -- >>> perfectShuffle [1,2,3,4] [5,6]
  -- [1,5,2,6,3,4]
  -- >>> perfectShuffle [1,2] [5,6,7,8]
  -- [1,5,2,6,7,8]
  perfectShuffle :: [a] -> [a] -> [a]
  perfectShuffle xs ys = xys ++ zs
    where
      nX  = L.length xs
      nY  = L.length ys
      k   = min nX nY
      xs' = L.take k xs
      ys' = L.take k ys
      xys = F.foldMap (\(x,y) -> [x,y]) $ zip xs' ys'
      zs  = if nX < nY then L.drop k ys else L.drop k xs

  reversal :: Int -> Int -> [a] -> [a]
  reversal i j xs = ps ++ L.reverse ys ++ ss
    where
      (ps, ss') = L.splitAt i xs
      (ys, ss)  = L.splitAt (j-i+1) ss'

  reversal' :: Int -> Int -> [a] -> [a]
  reversal' i m = reversal i (i+m-1)

  prefixReversal :: Int -> [a] -> [a]
  prefixReversal m = reversal 0 (m-1)


  transposition :: Int -> Int -> Int -> [a] -> [a]
  transposition i j k xs = ps ++ zs ++ ys ++ ss
    where
      (ps, ss')  = L.splitAt i xs
      (ys, ss'') = L.splitAt (j-i+1) ss'
      (yz, ss)   = L.splitAt (k-j-i+1) ss'
