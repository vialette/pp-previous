module Data.Algorithm.PP.Interval
  (
    -- * Type
      Interval(..)

    -- * Constructing
    , mk
    , mkSafe
    , union
    , intersection
    , cover

    -- * Querying
    , getLeft
    , getRight
    , len
    , disjoint

    -- * Transforming
    , ints
  ) where

import qualified Data.Foldable as F

newtype Interval = Interval (Int, Int) deriving (Show, Eq, Ord)


{- | 'mk' @l@ @r@ returns the interval with left endpoint @l@ and right endpoint @r@ if
@i <= r@. The raise raises an error if @l > r@.

>>> Interval.mk 2 5
Interval (2,5)
>>> Interval.mk 5 2
Interval *** Exception: bad interval.
-}
mk :: Int -> Int -> Interval
mk l r = case mkSafe l r of
                Nothing -> error "bad interval."
                Just i  -> i

{- | 'mk' @l@ @r@ returns the interval with left endpoint @l@ and right endpoint @r@
if @l <= r@ and @Nothing otherwhise.

>>> Interval.mkSafe 2 5
Just (Interval (2,5))
>>> Interval.mkSafe 5 2
Nothing
-}
mkSafe :: Int -> Int -> Maybe Interval
mkSafe l r
  | l > r     = Nothing
  | otherwise = Just $ Interval (l, r)

{- | 'getLeft' @i@ returns the left endpoint of interval @i@.

>>> let i = Interval.mk 2 5 in Interval.getLeft i
2
-}
getLeft :: Interval -> Int
getLeft (Interval (l, _)) = l

{- | 'getRight' @i@ returns the left endpoint of interval @i@.

>>> let i = Interval.mk 2 5 in Interval.getRight i
5
-}
getRight :: Interval -> Int
getRight (Interval (_, r)) = r

{- | 'union' @i@ @i'@ returns the union of intervals @i@ and @i'@ if they are intersecting.
Otherwise, the function returns @Nothing@.

-}
union :: Interval -> Interval -> Maybe Interval
union i@(Interval (l, r)) i'@(Interval (l', r'))
  | disjoint i i' = Nothing
  | otherwise     = mkSafe (min l l') (max r r')

{- | 'intersection' @i@ @i'@ returns the intersection of intervals @i@ and @i'@ if they are intersecting.
Otherwise, the function returns @Nothing@.

>>>
-}
intersection :: Interval -> Interval -> Maybe Interval
intersection i@(Interval (l, r)) i'@(Interval (l', r'))
  | disjoint i i' = Nothing
  | otherwise     = mkSafe (max l l') (min r r')

{- | 'cover' @is@ returns the least interval coverings all intervals in @is@.
The function returns @Nothing@ if @is@ is the empty list.

>>>
-}
cover :: [Interval] -> Maybe Interval
cover []       = Nothing
cover (i : is) = Just $ F.foldr f i is
  where
    f (Interval (l, r)) (Interval (l', r')) = mk (min l l') (max r r')

{- | 'len' @i@ returns the length of the interval @i@.

>>> Interval.mk 2 5 >>= (Just . Interval.len)
Just 3
>>> Interval.mk 2 2 >>= (Just . Interval.len)
Just 0
-}
len :: Interval -> Int
len (Interval (l, r)) = r-l

{- | 'disjoint' @i@ @i'@ returns return true iff the intervals @i@ and @i'@ are disjoint.

>>>
-}
disjoint :: Interval -> Interval -> Bool
disjoint (Interval (l, r)) (Interval (l', r')) = r < l' || r' < l

{- | 'ints' @i@ returns the list @[l, l+1, ..., r]@

>>> Interval.mk 2 5 >>= (Just . Interval.ints)
Just [2,3,4,5]
>>> Interval.mk 2 2 >>= (Just . Interval.ints)
Just [2]
-}
ints :: Interval -> [Int]
ints (Interval (l, r)) = [l .. r]