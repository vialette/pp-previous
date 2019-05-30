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

>>> Interval.union (Interval.mk 2 5) (Interval.mk 3 7)
Just (Interval (2,7))
>>> Interval.union (Interval.mk 2 5) (Interval.mk 6 7)
Nothing
-}
union :: Interval -> Interval -> Maybe Interval
union i@(Interval (l, r)) i'@(Interval (l', r'))
  | disjoint i i' = Nothing
  | otherwise     = mkSafe (min l l') (max r r')

{- | 'intersection' @i@ @i'@ returns the intersection of intervals @i@ and @i'@ if they are intersecting.
Otherwise, the function returns @Nothing@.

>>> Interval.intersection (Interval.mk 2 5) (Interval.mk 3 7)
Just (Interval (3,5))
>>> Interval.intersection (Interval.mk 2 5) (Interval.mk 6 7)
Nothing
-}
intersection :: Interval -> Interval -> Maybe Interval
intersection i@(Interval (l, r)) i'@(Interval (l', r'))
  | disjoint i i' = Nothing
  | otherwise     = mkSafe (max l l') (min r r')

{- | 'cover' @is@ returns the least interval coverings all intervals in @is@.
The function returns @Nothing@ if @is@ is the empty list.

>>> Interval.cover [Interval.mk 2 5, Interval.mk 3 7, Interval.mk 6 8]
Just (Interval (2,8))
>>> Interval.cover []
Nothing
-}
cover :: [Interval] -> Maybe Interval
cover []       = Nothing
cover (i : is) = Just $ F.foldr f i is
  where
    f (Interval (l, r)) (Interval (l', r')) = mk (min l l') (max r r')

{- | 'len' @i@ returns the length of the interval @i@.

>>> let i = Interval.mk 2 5 in Interval.len i
3
>>> let i = Interval.mk 2 2 in Interval.len i
0
-}
len :: Interval -> Int
len (Interval (l, r)) = r-l

{- | 'disjoint' @i@ @i'@ returns return true iff the intervals @i@ and @i'@ are disjoint.

>>> let i = Interval.mk 2 5; i' = Interval.mk 3 7 in Interval.disjoint i i'
False
>>> let i = Interval.mk 2 5; i' = Interval.mk 6 7 in Interval.disjoint i i'
True
-}
disjoint :: Interval -> Interval -> Bool
disjoint (Interval (l, r)) (Interval (l', r')) = r < l' || r' < l

{- | 'ints' @i@ returns the list @[l, l+1, ..., r]@

>>> let i = Interval.mk 2 5 in Interval.ints i
[2,3,4,5]
-}
ints :: Interval -> [Int]
ints (Interval (l, r)) = [l .. r]