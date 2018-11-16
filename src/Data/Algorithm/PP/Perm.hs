module Data.Algorithm.PP.Perm
(
  Perm
, T

, identity
, fromList
, mk

, toList

, perms

, len
, at

, inv
, rev
, comp
, revComp

, sub
)
where

  import qualified Data.List      as L
  import qualified Data.Foldable  as F
  import qualified Data.Tuple     as T
  import Data.Function (on)

  import qualified Data.Algorithm.PP.Combi      as PP.Combi
  import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List

  type T = Int

  newtype Perm = PermImpl { getElems :: [T] } deriving (Eq, Ord)

  instance Show Perm where
    show = show . getElems

  -- | 'toList p' return the list of the elements of permutation 'p'.
  toList = getElems

  --
  reduce :: (Ord a) => [a] -> [T]
  reduce = L.map T.fst . L.sortBy cmpFstSnd . L.zip [1..] . L.sortBy cmpSnd . L.zip [1..]
    where
      cmpFstSnd = compare `on` (T.fst . T.snd)
      cmpSnd    = compare `on` T.snd

  --
  fromList :: (Ord a) => [a] -> Perm
  fromList = PermImpl . reduce

  -- | Alias for 'fromList'.
  mk :: (Ord a) => [a] -> Perm
  mk = fromList

  -- | 'identity n' retuns the identity permutation of length 'n'.
  identity :: Int -> Perm
  identity n = PermImpl [1 .. n]

  -- | 'perms n' returns all permutations of length 'n'.
  perms :: Int -> [Perm]
  perms n = L.map PermImpl $ L.permutations [1..n]

  -- | 'len p' returns the length of permutation 'p'.
  len :: Perm -> Int
  len = L.length . getElems

  -- | 'p `at` i' returns the element at position 'i' in permutation 'p'.
  at :: Perm -> Int -> T
  at p i = xs L.!! i
    where
      xs = getElems p

  -- | 'inv p' returns the inverse of permutation 'p'.
  inv :: Perm -> Perm
  inv PermImpl { getElems = xs } = PermImpl . L.map T.snd . L.sort $ L.zip xs [1..L.length xs]

  -- | 'rev p' returns the reverse of permutation 'p'.
  rev :: Perm -> Perm
  rev = PermImpl . L.reverse . getElems

  -- | 'comp p' returns the complement of permutation 'p'.
  comp :: Perm -> Perm
  comp PermImpl { getElems = xs } = PermImpl $ fmap (\x -> m - x + 1) xs
    where
      m = F.maximum xs

  -- | 'revComp p' returns the reverse complement of permutation 'p'.
  revComp :: Perm -> Perm
  revComp = rev . comp

  -- | 'sub' 'k' 'p' returns all permutations of length 'k' that occurs in
  -- permutation 'p'.
  sub :: Int -> Perm -> [Perm]
  sub k = PP.Utils.List.uniq . L.map mk . PP.Combi.subsets k . toList
