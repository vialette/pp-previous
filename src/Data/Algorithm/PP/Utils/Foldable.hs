module Data.Algorithm.PP.Utils.Foldable (
  -- * Maximum
    maximumsBy
  , maximumBy
  , maximumBy0
  , maximumBy1

  -- * Splitting
  , splitEvery
  ) where

import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T

import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List

maximumsBy _ [] = []
maximumsBy f xs = T.snd . F.foldr1 g $ fmap (\ x -> (f x, [x])) xs
  where
    g (k, [x]) (maxSoFar, acc)
      | k > maxSoFar  = (k,        [x])
      | k == maxSoFar = (maxSoFar, x : acc)
      | otherwise     = (maxSoFar, acc)

{- |'maximumBy' @f@ @z@ @e@ @xs@ returns the pair @(m, xs')@ where
@m = maximum (fmap f xs)@ and @xs'@ are the elements of @xs'@ that attain
maximum @m@.

>>> maximumBy length 0 [] [[1],[2..3],[4..6],[7],[8..9],[10..12]]
(3,[[4,5,6],[10,11,12]])
>>> maximumBy head 0 [] [[1],[2..3],[4..6],[7],[8..9],[10..12]]
(10,[[10,11,12]])
-}
maximumBy :: (Foldable t, Ord b) => (a -> b) -> b -> [a] -> t a -> (b, [a])
maximumBy f z e = F.foldr aux (z, e)
  where
    aux x (m, acc)
      | k > m     = (k, [x])
      | k == m    = (m, x : acc)
      | otherwise = (m, acc)
        where
          k = f x

{- |
-}
maximumBy0 :: (Foldable t, Ord b, Num b) => (a -> b) -> t a -> (b, [a])
maximumBy0 f = maximumBy f 0 []

{- |
-}
maximumBy1 :: (Foldable t, Ord b, Num b) => (a -> b) -> t a -> (b, [a])
maximumBy1 f = maximumBy f 1 []



{- | 'splitEvery' @p@ @xs@ splits foldable @xs@ after each element @x@ such that @p x@ is @True@.

>>> splitEvery even [1..6]
[[1,2],[3,4],[5,6]]
>>> splitEvery even [1..7]
[[1,2],[3,4],[5,6],[7]]
>>> splitEvery odd [1..6]
[[1],[2,3],[4,5],[6]]
>>> splitEvery odd [1..7]
[[1],[2,3],[4,5],[6,7]]
-}
splitEvery :: Foldable t => (a -> Bool) -> t a -> [[a]]
splitEvery p = (\ (xs, xss) -> if L.null xs then xss else xs : xss) . F.foldr f ([], [])
  where
    f x (currentAcc, acc)
      | p x       = if L.null currentAcc then ([x], acc) else ([x], currentAcc : acc)
      | otherwise = (x : currentAcc, acc)
