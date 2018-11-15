module Data.Algorithm.PP.Perm
(
  Perm

, identity
, fromList

, toList

, perms

, inv
, rev
, comp
, revComp
)
where

  import qualified Data.List      as L
  import qualified Data.Foldable  as F
  import qualified Data.Tuple     as T
  import Data.Function (on)

  newtype Perm = Perm { getElems :: [Int] } deriving (Eq, Ord)

  instance Show Perm where
    show = show . getElems

  toList = getElems

  --
  reduce :: (Ord a) => [a] -> [Int]
  reduce = L.map T.fst . L.sortBy cmpFstSnd . L.zip [1..] . L.sortBy cmpSnd . L.zip [1..]
    where
      cmpFstSnd = compare `on` (T.fst . T.snd)
      cmpSnd    = compare `on` T.snd

  --
  fromList :: (Ord a) => [a] -> Perm
  fromList = Perm . reduce

  --
  identity :: Int -> Perm
  identity n = Perm [1 .. n]

  -- 'perms n' returns all permutations of length 'n'.
  perms :: Int -> [Perm]
  perms n = L.map Perm $ L.permutations [1..n]

  len :: Perm -> Int
  len = L.length . getElems

  -- The 'inv' function returns the inverse permutation.
  inv :: Perm -> Perm
  inv Perm { getElems = xs } = Perm . L.map T.snd . L.sort $ L.zip xs [1..L.length xs]

  -- 'rev p' returns the reverse of permutation 'p'.
  rev :: Perm -> Perm
  rev = Perm . L.reverse . getElems

  -- 'comp p' returns the complement of permutation 'p'.
  comp :: Perm -> Perm
  comp Perm { getElems = xs } = Perm $ fmap (\x -> m - x + 1) xs
    where
      m = F.maximum xs

  -- 'revComp p' returns the reverse complement of permutation 'p'.
  revComp :: Perm -> Perm
  revComp = rev . comp
