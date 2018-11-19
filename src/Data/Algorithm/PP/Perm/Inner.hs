module Data.Algorithm.PP.Perm.Inner
(
  T
, Perm(..)
, FPerm

, mk
, fromList

, toList
)
where

  import qualified Data.List      as L
  import qualified Data.Foldable  as F
  import qualified Data.Tuple     as T
  import Data.Function (on)

  -- |
  type T = Int

  -- |'Perm' type
  newtype Perm = PermImpl { getElems :: [T] } deriving (Eq, Ord)

  -- |
  type FPerm = Perm -> Perm

  -- |
  instance Show Perm where
    show = show . getElems

  -- | 'toList' 'p' returns the list of the elements of permutation 'p'.
  toList = getElems

  -- Reduce a list.
  reduce :: (Ord a) => [a] -> [T]
  reduce = L.map T.fst . L.sortBy cmpFstSnd . L.zip [1..] . L.sortBy cmpSnd . L.zip [1..]
    where
      cmpFstSnd = compare `on` (T.fst . T.snd)
      cmpSnd    = compare `on` T.snd

  -- |'fromList' 'xs' makes a permutation from the list 'xs'.
  -- Duplicate elements in 'xs' are ordered from left to right.
  --
  -- >>> fromList "acba"
  -- [1,4,3,2]
  -- >>> fromList [2,9,7,2]
  -- [1,4,3,2]
  fromList :: (Ord a) => [a] -> Perm
  fromList = PermImpl . reduce

  -- | Alias for 'fromList'.
  mk :: (Ord a) => [a] -> Perm
  mk = fromList
