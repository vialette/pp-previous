module Data.Algorithm.PP.Perm
(
  -- * Types
  module Data.Algorithm.PP.Perm.Intenal

  -- * Points
, module Data.Algorithm.PP.Geometry.Point

  -- * Making
, identity
, mkPerm
, mkPatt

  -- * Transforming
, PP.Perm.Internal.getList
, PP.Perm.Internal.getPoints
, PP.Perm.Internal.getCoords

  -- * Generating
, perms

  -- Querying
, len

-- * Trivial bijections
, inv
, rev
, comp

-- * Composing trivial bijections
, revComp
, compRev
, invRev
, compInv
, invComp
, revInv
, invRevComp

-- , transpose

, extendLeft
, extendRight

, prefix
, prefixes
, suffix
, suffixes

, patterns
, patterns'
, maxpatterns'
, factors
, factors'
, maxfactors'
, partitions

  -- * Displaying
, grid
, grid'
)
where

  import qualified Control.Arrow  as A
  import qualified Data.List      as L
  import qualified Data.Foldable  as F
  import qualified Data.Tuple     as T
  import Data.Function (on)

  import Data.Algorithm.PP.Geometry.Point (X, Y, Point)
  import qualified Data.Algorithm.PP.Perm.Internal  as PP.Perm.Internal
  import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point
  import qualified Data.Algorithm.PP.Combi          as PP.Combi
  import qualified Data.Algorithm.PP.Utils.List     as PP.Utils.List

  -- | 'mkPerm' 'ps'
  --
  -- >>>
  mkPerm :: (Foldable t, Ord a) => t a -> PP.Perm.Internal.Perm
  mkPerm = PP.Perm.Internal.mkPerm . PP.Geometry.Point.mks [1..] . reduce . F.toList

  -- | 'mkPatt' 'ps'
  --
  -- >>>
  mkPatt :: [Point] -> PP.Perm.Internal.Patt
  mkPatt = PP.Perm.Internal.mkPatt

  -- Reduce a list of elements.
  reduce :: (Ord a) => [a] -> [Y]
  reduce = L.map T.fst . L.sortBy cmpFstSnd . L.zip [1..] . L.sortBy cmpSnd . L.zip [1..]
    where
      cmpFstSnd = compare `on` (T.fst . T.snd)
      cmpSnd    = compare `on` T.snd

  -- | 'identity' 'n' returns the identity permutation of length 'n'.
  --
  -- >>> identity 4
  -- [1,2,3,4]
  identity :: Int -> PP.Perm.Internal.Perm
  identity n = mkPerm $ PP.Geometry.Point.mks [1..n] [1..n]

  -- |'empty' eturns the empty permutation.
  empty :: PP.Perm.Internal.Perm
  empty = mkPerm []

  -- | 'perms' 'n' returns all permutations of length 'n'.
  --
  -- >>> perms 0
  -- [[]]
  -- >>> perms 1
  -- [[1]]
  -- >>> perms 2
  -- [[1,2],[2,1]]
  -- >>> perms 3
  -- [[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]
  perms :: Int -> [PP.Perm.Internal.Perm]
  perms n = L.map mkPerm $ L.permutations [1..n]

  -- | 'inv' 'p' returns the inverse of the permutation 'p'.
  --
  -- prop> inv (inv p) = p
  --
  -- >>> inv $ mk [1,3,4,2]
  -- [1,4,2,3]
  inv :: PP.Perm.Internal.Perm -> PP.Perm.Internal.Perm
  inv = mkPerm . L.map PP.Geometry.Point.getX . L.sortBy PP.Geometry.Point.cmpX . L.map PP.Geometry.Point.symm . PP.Perm.Internal.getPoints

  -- | 'rev' 'p' returns the reverse of the permutation 'p'.
  --
  -- prop> rev (rev p) = p
  --
  -- >>> rev $ mk [1,3,4,2]
  -- [2,4,3,1]
  rev :: PP.Perm.Internal.Perm -> PP.Perm.Internal.Perm
  rev = mkPerm . L.reverse . PP.Perm.Internal.getList

  -- | 'comp' 'p' returns the complement of the permutation 'p'.
  --
  -- prop> comp (comp p) = p
  --
  -- >>> comp $ mk [1,3,4,2]
  -- [4,2,1,3]
  comp :: PP.Perm.Internal.Perm -> PP.Perm.Internal.Perm
  comp p = mkPerm $ fmap (\y -> m-y+1) ys
    where
      ys = PP.Perm.Internal.getList p
      m  = F.maximum ys

  -- | 'revComp' 'p' returns the reverse complement of the permutation 'p'.
  --
  -- >>> revComp $ mk [1,3,4,2]
  -- [3,1,2,4]
  revComp :: PP.Perm.Internal.Perm -> PP.Perm.Internal.Perm
  revComp = rev . comp

  -- | 'compRev' 'p' returns the complement reverse of the permutation 'p'.
  --
  -- prop> revComp p == compRev p
  compRev :: PP.Perm.Internal.Perm -> PP.Perm.Internal.Perm
  compRev = revComp

  -- | 'compInv' 'p' returns the complement inverse of the permutation 'p'.
  --
  -- >>> compInv $ mk [1,3,4,2]
  -- [4,1,3,2]
  compInv :: PP.Perm.Internal.Perm -> PP.Perm.Internal.Perm
  compInv = comp . inv

  -- | 'invRev' 'p' returns the inverset inverse of the permutation 'p'.
  --
  -- prop> invRev p == compRev
  invRev :: PP.Perm.Internal.Perm -> PP.Perm.Internal.Perm
  invRev = comp . inv

  -- | 'invComp' 'p' returns the inverse complement of the permutation 'p'.
  --
  -- >>> invComp $ mk [1,3,4,2]
  -- [3,2,4,1]
  invComp :: PP.Perm.Internal.Perm -> PP.Perm.Internal.Perm
  invComp = inv . comp

  -- | 'revInv' 'p' returns the reverse inverse of the permutation 'p'.
  --
  -- prop> revInv p == invComp p
  revInv :: PP.Perm.Internal.Perm -> PP.Perm.Internal.Perm
  revInv = invComp

  -- | 'invRevComp' 'p' returns the inverse reverse complement of permutation 'p'.
  --
  -- >>> invRevComp $ mk [1,3,4,2]
  -- [2,3,1,4
  invRevComp :: PP.Perm.Internal.Perm -> PP.Perm.Internal.Perm
  invRevComp = inv . rev . comp

  -- transpose :: Int -> Int -> Perm -> Perm
  -- transpose i j = mk $ L.prefix (i-1) xs ++ [xs L.!! i] ++

  -- |'extendLeft' 'p' takes a permutation 'p' of length 'n' and returns all
  -- permutations of length 'n'+1 which suffix of length 'n' is order-isomorphic
  -- to 'p'.
  --
  -- >>> extendLeft $ mk [2,1,3]
  -- [[1,3,2,4],[2,3,1,4],[3,2,1,4],[4,2,1,3]]
  extendLeft :: PP.Perm.Internal.Perm -> [PP.Perm.Internal.Perm]
  extendLeft p = L.map mkPerm [k : f k ys | k <- [1..PP.Perm.Internal.len p+1]]
    where
      ys  = PP.Perm.Internal.getList p
      f k = F.foldr (\y acc -> (if y<k then y else y+1) : acc) []

  -- |'extendRight' 'p' takes a permutation 'p' of length 'n' and returns all
  -- permutations of length 'n'+1 which prefix of length 'n' is order-isomorphic
  -- to 'p'.
  --
  -- >>> extendRight $ mk [2,1,3]
  -- [[3,2,4,1],[3,1,4,2],[2,1,4,3],[2,1,3,4]]
  extendRight :: PP.Perm.Internal.Perm -> [PP.Perm.Internal.Perm]
  extendRight = L.map rev . extendLeft . rev

  -- |'grid' 'p'
  --
  -- >>> putStr . grid  $ PP.Perm.mk [5,1,6,4,2,3]
  -- +---+---+---+---+---+---+
  -- |   |   | o |   |   |   |
  -- +---+---+---+---+---+---+
  -- | o |   |   |   |   |   |
  -- +---+---+---+---+---+---+
  -- |   |   |   | o |   |   |
  -- +---+---+---+---+---+---+
  -- |   |   |   |   |   | o |
  -- +---+---+---+---+---+---+
  -- |   |   |   |   | o |   |
  -- +---+---+---+---+---+---+
  -- |   | o |   |   |   |   |
  -- +---+---+---+---+---+---+
  grid :: PP.Perm.Internal.Perm -> String
  grid p = aux . L.map (row . PP.Geometry.Point.getX) . L.sortBy (flip PP.Geometry.Point.cmpY) $ PP.Perm.Internal.getPoints p
    where
      n     = PP.Perm.Internal.len p
      sep   = ('+' :) $ F.concat (L.replicate n "---+") ++ "\n"
      row x = ('|' :) $ F.concat [F.concat (L.replicate (x-1) "   |"), " o |", F.concat (L.replicate (n-x) "   |"), "\n"]
      aux ss = sep ++ L.intercalate sep ss ++ sep

  grid' :: PP.Perm.Internal.Perm -> String
  grid' p = ""
