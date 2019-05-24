module Data.Algorithm.PP.Interval
  (
    -- * Type
      Interval(..)

    -- * Constructing
    , mk
    , mkSafe
    , union
    , intersection

    -- * Querying
    , len
    , disjoint
  ) where

data Interval = Interval { getLeft :: Int, getRight :: Int } deriving (Show, Eq, Ord)

mk :: Int -> Int -> Interval
mk l r = Interval { getLeft = l, getRight = r }

mkSafe :: Int -> Int -> Maybe Interval
mkSafe l r
  | l > r = Nothing
  | otherwise = Just $ mk l r


union :: Interval -> Interval -> Maybe Interval
union i i'
  | disjoint i i' = Nothing
  | otherwise     = mkSafe l'' r''
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
  | otherwise     = mkSafe l'' r''
  where
    l   = getLeft i
    r   = getRight i
    l'  = getLeft i'
    r'  = getRight i'
    l'' = max l l'
    r'' = min r r'

len :: Interval -> Int
len i = getRight i - getLeft i

disjoint ::  Interval -> Interval -> Bool
disjoint i i' = r < l' || r' < l
  where
    l   = getLeft i
    r   = getRight i
    l'  = getLeft i'
    r'  = getRight i'