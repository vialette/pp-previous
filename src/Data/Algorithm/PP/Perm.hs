module Data.Algorithm.PP.Perm
(
  Perm
, T

, identity
, fromList
, mk

, toList
, toPoints

, perms

, len
, at

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

  import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point

  type T = Int

  newtype Perm = PermImpl { getElems :: [T] } deriving (Eq, Ord)

  instance Show Perm where
    show = show . getElems

  toList = getElems

  toPoints = L.map (T.uncurry PP.Geometry.Point.mk). zip [1..] . toList

  --
  reduce :: (Ord a) => [a] -> [T]
  reduce = L.map T.fst . L.sortBy cmpFstSnd . L.zip [1..] . L.sortBy cmpSnd . L.zip [1..]
    where
      cmpFstSnd = compare `on` (T.fst . T.snd)
      cmpSnd    = compare `on` T.snd

  --
  fromList :: (Ord a) => [a] -> Perm
  fromList = PermImpl . reduce

  mk :: (Ord a) => [a] -> Perm
  mk = fromList

  --
  identity :: Int -> Perm
  identity n = PermImpl [1 .. n]

  -- 'perms n' returns all permutations of length 'n'.
  perms :: Int -> [Perm]
  perms n = L.map PermImpl $ L.permutations [1..n]

  len :: Perm -> Int
  len = L.length . getElems

  at :: Perm -> Int -> T
  at p i = xs L.!! i
    where
      xs = getElems p

  -- The 'inv' function returns the inverse permutation.
  inv :: Perm -> Perm
  inv PermImpl { getElems = xs } = PermImpl . L.map T.snd . L.sort $ L.zip xs [1..L.length xs]

  -- 'rev p' returns the reverse of permutation 'p'.
  rev :: Perm -> Perm
  rev = PermImpl . L.reverse . getElems

  -- 'comp p' returns the complement of permutation 'p'.
  comp :: Perm -> Perm
  comp PermImpl { getElems = xs } = PermImpl $ fmap (\x -> m - x + 1) xs
    where
      m = F.maximum xs

  -- 'revComp p' returns the reverse complement of permutation 'p'.
  revComp :: Perm -> Perm
  revComp = rev . comp
