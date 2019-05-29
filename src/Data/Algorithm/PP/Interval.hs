module Data.Algorithm.PP.Interval
  (
    -- * Type
      Interval(..)

    -- * Constructing
    , mk
    , mkUnsafe
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


mkUnsafe :: Int -> Int -> Interval
mkUnsafe l r = Interval (l, r)

{- | 'mk' @l@ @r@ returns the interval with left endpoint @l@ and right endpoint @r@
if @l <= r@ and @Nothing otherwhise.

>>> Interval.mk 2 6

-}
mk :: Int -> Int -> Maybe Interval
mk l r
  | l > r     = Nothing
  | otherwise = Just $ mkUnsafe l r

getLeft :: Interval -> Int
getLeft (Interval (l, _)) = l

getRight :: Interval -> Int
getRight (Interval (_, r)) = r

union :: Interval -> Interval -> Maybe Interval
union i@(Interval (l, r)) i'@(Interval (l', r'))
  | disjoint i i' = Nothing
  | otherwise     = mk l'' r''
  where
    l'' = min l l'
    r'' = max r r'

intersection :: Interval -> Interval -> Maybe Interval
intersection i@(Interval (l, r)) i'@(Interval (l', r'))
  | disjoint i i' = Nothing
  | otherwise     = mk l'' r''
  where
    l'' = max l l'
    r'' = min r r'

cover :: [Interval] -> Maybe Interval
cover []       = Nothing
cover (i : is) = Just $ F.foldr f i is
  where
    f (Interval (l, r)) (Interval (l', r')) = mkUnsafe l'' r''
      where
        l'' = min l l'
        r'' = max r r'

len :: Interval -> Int
len (Interval (l, r)) = r-l

disjoint ::  Interval -> Interval -> Bool
disjoint (Interval (l, r)) (Interval (l', r')) = r < l' || r' < l

ints :: Interval -> [Int]
ints (Interval (l, r)) = [l .. r]