module Data.Algorithm.PP.Utils.Foldable (
  -- * minimum / maximum
    minimumsBy
  , maximumsBy

  -- * Splitting
  , splitEvery
  ) where

import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T

import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List

-- maximumsBy and minimumsBy helper function.
extremalsBy :: (Foldable t, Functor t, Eq b) => (b -> b -> Bool) -> (a -> b) -> t a -> (b, [a])
extremalsBy cmp f = F.foldr1 g . fmap (\ x -> (f x, [x]))
  where
    g (k, [x]) (l, acc)
      | k `cmp` l = (k, [x])
      | k == l    = (l, x : acc)
      | otherwise = (l, acc)

{- | 'maximumsBy' @f@ @xs@ returns the list of the maximum elements of @xs@
(according to function @f@) together with the maximum.

-}
maximumsBy :: (Foldable t, Functor t, Ord a, Ord b, Eq b) => (a -> b) -> t a -> (b, [a])
maximumsBy = extremalsBy (>)

{- | 'maximumsBy' @f@ @xs@ returns the list of the maximum elements of @xs@
(according to function @f@) together with the maximum.

-}
minimumsBy :: (Foldable t, Functor t, Ord a, Ord b, Eq b) => (a -> b) -> t a -> (b, [a])
minimumsBy = extremalsBy (<)


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
