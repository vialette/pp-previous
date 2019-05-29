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
    , len
    , disjoint

    -- * Transforming
    , ints
  ) where

import qualified Data.Foldable as F

data Interval = Interval { getLeft :: Int, getRight :: Int } deriving (Show, Eq, Ord)

mkUnsafe :: Int -> Int -> Interval
mkUnsafe l r = Interval { getLeft = l, getRight = r }

mk :: Int -> Int -> Maybe Interval
mk l r
  | l > r     = Nothing
  | otherwise = Just $ mkUnsafe l r


union :: Interval -> Interval -> Maybe Interval
union i i'
  | disjoint i i' = Nothing
  | otherwise     = mk l'' r''
  where
    l   = getLeft i
    r   = getRight i
    l'  = getLeft i'
    r'  = getRight i'
    l'' = min l l'
    r'' = max r r'

intersection :: Interval -> Interval -> Maybe Interval
intersection i i'
  | disjoint i i' = Nothing
  | otherwise     = mk l'' r''
  where
    l   = getLeft i
    r   = getRight i
    l'  = getLeft i'
    r'  = getRight i'
    l'' = max l l'
    r'' = min r r'

cover :: [Interval] -> Maybe Interval
cover []       = Nothing
cover (i : is) = Just $ F.foldr f i is
  where
    f i iCover = mkUnsafe l r
      where
        l = min (getLeft i)  (getLeft iCover)
        r = max (getRight i) (getRight iCover)

len :: Interval -> Int
len i = getRight i - getLeft i

disjoint ::  Interval -> Interval -> Bool
disjoint i i' = r < l' || r' < l
  where
    l   = getLeft i
    r   = getRight i
    l'  = getLeft i'
    r'  = getRight i'

ints :: Interval -> [Int]
ints i = [getLeft i .. getRight i]