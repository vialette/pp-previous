module Data.Algorithm.PP.Perm
(
  -- * Type
  Perm
, Patt
, FPerm
, Point

-- * Building
, mkPerm
, mkPermUnsafe
, mkPatt
, identity

-- * Transforming
, getPoints
, getList

-- * Comparing
, orderIso

-- * Querying
, len

, prefix
, prefixes
, suffix
, suffixes
, factors
, factors'
, maxfactors
, patterns
, patterns'
, maxPatterns

, partitions

, inversions


  -- * Generating
, perms


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

-- * Querying

  -- * Extending
, extendLeft
, extendRight

  -- * Displaying
, grid
, grid'
)
where

  import qualified Control.Arrow as A
  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Tuple    as T
  import Data.Function (on)

  -- import Data.Algorithm.PP.Geometry.Point (X, Y, Point)
  --import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point
  import qualified Data.Algorithm.PP.Combi          as PP.Combi
  import qualified Data.Algorithm.PP.Utils.List     as PP.Utils.List

  -- |'Point' type
  type Point = (Int, Int)

  -- |'Seq' type
  newtype P a = P { getPoints :: [Point] }

  -- |
  instance Show (P a) where
    show = show . L.map T.snd . getPoints

  -- |
  instance Eq (P a) where
    p == q = f p == f q
      where
        f = getList. mkPerm . getPoints

  -- |
  instance Ord (P a) where
    p `compare` q = f p `compare` f q
      where
        f = getList. mkPerm . getPoints

  -- |'Span' type
  data Span

  -- |'Sub' type
  data Sub

  -- |'Perm' type
  type Perm = P Span

  -- |'Patt' type
  type Patt = P Sub

  type FPerm = Perm -> Perm

  -- | 'mkPerm' 'xs'
  --
  -- >>>
  mkPerm :: (Foldable t, Ord a) => t a -> Perm
  mkPerm = mkPermUnsafe . reduce . F.toList

  -- |'mkPermUnsafe' 'xs'
  mkPermUnsafe :: [Int] -> Perm
  mkPermUnsafe = P . L.zip [1..]

  -- Reduce a list of elements.
  reduce :: (Ord a) => [a] -> [Int]
  reduce = L.map T.fst . L.sortBy cmpFstSnd . L.zip [1..] . L.sortBy cmpSnd . L.zip [1..]
    where
      cmpFstSnd = compare `on` (T.fst . T.snd)
      cmpSnd    = compare `on` T.snd

  -- | 'mkPatt' 'ps'
  --
  -- >>>
  mkPatt :: [Point] -> Patt
  mkPatt = P

  -- | 'getList' 'p' returns the list of the elements of permutation 'p'.
  getList :: P a -> [Int]
  getList = L.map T.snd . getPoints

  -- |'orderIso' 'p' 'q'
  orderIso :: Perm -> Patt -> Bool
  orderIso p = (==) p . P . getPoints

  -- | 'len' 'p' returns the length of the permutation 'p'.
  len :: P a -> Int
  len = L.length . getPoints

  -- 'prefix' 'k' 'p'
  --
  -- >>> p = mk [2,4,5,1,6,3]
  -- >>> [prefix i p | i <- [1..len p]]
  -- [[1],[1,2],[1,2,3],[2,3,4,1],[2,3,4,1,5],[2,4,5,1,6,3]]
  prefix :: Int -> P a -> Patt
  prefix k = P . L.take k . getPoints

  -- |'prefixes' 'p' returns all the prefixes
  --
  -- >>> p = mk [2,4,5,1,6,3]
  -- >>> prefixes p
  -- [[1],[1,2],[1,2,3],[2,3,4,1],[2,3,4,1,5],[2,4,5,1,6,3]]
  prefixes :: P a -> [Patt]
  prefixes = L.map P . L.tail . L.inits . getPoints

  -- 'suffix' 'k' 'p' returns the suffix of length 'k' of the permutation 'p' as
  -- a permutation.
  --
  -- >>> p = mk [2,4,5,1,6,3]
  -- >>> [suffix i p | i <- [1..len p]]
  -- [[1],[2,1],[1,3,2],[3,1,4,2],[3,4,1,5,2],[2,4,5,1,6,3]]
  suffix :: Int -> P a -> Patt
  suffix k p = P . L.drop (n-k) $ getPoints p
    where
      n = len p

  -- |'suffixes' 'p' returns all the suffixes of the permutation 'p' as
  -- permutations.
  --
  -- >>> p = mk [2,4,5,1,6,3]
  -- >>> suffixes p
  -- [[2,4,5,1,6,3],[3,4,1,5,2],[3,1,4,2],[1,3,2],[2,1],[1]]
  suffixes :: P a -> [Patt]
  suffixes = L.map P . L.init . L.tails . getPoints

  -- |'factors' 'k' 'p' returns the list of all factors of length 'k' of the
  -- permutation 'p'.
  --
  -- >>> p = mk [2,4,1,3]
  -- >>> factors 0 p
  -- []
  -- >>> factors 1 p
  -- [[1],[2],[3],[4]]
  -- >>> factors 2 p
  -- [[1,3],[2,4],[4,1]]
  -- >>> factors 3 p
  -- [[2,4,1],[4,1,3]]
  -- >>> factors 4 p
  -- [[2,4,1,3]]
  -- >>> factors 5 p
  -- []
  factors :: Int -> P a -> [Patt]
  factors k = L.map P . PP.Utils.List.chunk k . getPoints

  -- |'factors'' 'p'
  factors' :: P a -> [Patt]
  factors' p = F.concat [factors k p |  k <- [1..len p]]

  -- |'maxfactors' 'f' 'p'
  maxfactors :: (Patt -> Bool) -> P a -> [Patt]
  maxfactors f p = select $ L.dropWhile L.null [[q | q <- factors k p, f q] | k <- [n,n-1..1]]
    where
      n         = len p
      select xs = if L.null xs then [] else L.head xs

  -- | 'patterns' 'k' 'p' returns all distinct permutations of length 'k' that
  -- occurs in permutation 'p'.
  --
  -- >>> patterns 0 (mk [2,4,1,3,5])
  -- [[]]
  -- >>> patterns 1 (mk [2,4,1,3,5])
  -- [[1]]
  -- >>> patterns 2 (mk [2,4,1,3,5])
  -- [[1,2],[2,1]]
  -- >>> patterns 3 (mk [2,4,1,3,5])
  -- [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2]]
  -- >>> patterns 4 (mk [2,4,1,3,5])
  -- [[1,3,2,4],[2,1,3,4],[2,3,1,4],[2,4,1,3],[3,1,2,4]]
  -- >>> patterns 5 (mk [2,4,1,3,5])
  -- [[2,4,1,3,5]]
  -- >>> patterns 6 (mk [2,4,1,3,5])
  -- []
  patterns :: Int -> P a -> [Perm]
  patterns k = L.map P . PP.Combi.subsets k . getPoints

  -- |'patterns'' 'k' 'p'
  patterns' :: Int -> P a -> [Perm]
  patterns' k = L.map (P . getPoints) . patterns k

  -- |'maxPatterns' 'f' 'p' returns the longest patterns 'q' of permutation 'p'
  -- such that 'f' 'q' holds.
  maxPatterns :: (Perm-> Bool) -> P a -> [Perm]
  maxPatterns f p = select $ L.dropWhile L.null [[q | q <- patterns k p, f q] | k <- [n,n-1..1]]
    where
      n         = len p
      select ys = if L.null ys then [] else L.head ys

  -- |'partitions' 'p' 'k' 'l' returns all partitions ('qk','ql') of the permutation
  -- 'p' such that '|qk|=k' and '|ql|=l'
  --
  -- >>>
  partitions :: Int -> Int -> P a -> [(Patt, Patt)]
  partitions k l = L.map ((A.***) P P) . PP.Combi.partitions k l . getPoints

  -- |'inversions' 'p' returns the inversions of the permutation 'p'.
  --
  -- >>> inversions (mk [1,5,3,2,6,4])
  -- [(5,3),(5,2),(5,4),(3,2),(6,4)]
  inversions :: P a -> [(Int, Int)]
  inversions = L.map (\[i, j] -> (i, j)) . L.filter (\[i, j] -> i > j) . PP.Combi.subsets 2 . getList

  -- | 'identity' 'n' returns the identity permutation of length 'n'.
  --
  -- >>> identity 4
  -- [1,2,3,4]
  identity :: Int -> Perm
  identity n = mkPermUnsafe [1..n]

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
  perms :: Int -> [Perm]
  perms n = L.map mkPermUnsafe $ L.permutations [1..n]

  -- | 'inv' 'p' returns the inverse of the permutation 'p'.
  --
  -- prop> inv (inv p) = p
  --
  -- >>> inv $ mk [1,3,4,2]
  -- [1,4,2,3]
  inv :: Perm -> Perm
  inv = P . L.sortOn T.fst . L.map (\ (x, y) -> (y, x)) . getPoints

  -- | 'rev' 'p' returns the reverse of the permutation 'p'.
  --
  -- prop> rev (rev p) = p
  --
  -- >>> rev $ mk [1,3,4,2]
  -- [2,4,3,1]
  rev :: Perm -> Perm
  rev = mkPermUnsafe . L.reverse . getList

  -- | 'comp' 'p' returns the complement of the permutation 'p'.
  --
  -- prop> comp (comp p) = p
  --
  -- >>> comp $ mk [1,3,4,2]
  -- [4,2,1,3]
  comp :: Perm -> Perm
  comp p = mkPermUnsafe $ fmap (\y -> m-y+1) ys
    where
      ys = getList p
      m  = F.maximum ys

  -- | 'revComp' 'p' returns the reverse complement of the permutation 'p'.
  --
  -- >>> revComp $ mk [1,3,4,2]
  -- [3,1,2,4]
  revComp :: Perm -> Perm
  revComp = rev . comp

  -- | 'compRev' 'p' returns the complement reverse of the permutation 'p'.
  --
  -- prop> revComp p == compRev p
  compRev :: Perm -> Perm
  compRev = revComp

  -- | 'compInv' 'p' returns the complement inverse of the permutation 'p'.
  --
  -- >>> compInv $ mk [1,3,4,2]
  -- [4,1,3,2]
  compInv :: Perm -> Perm
  compInv = comp . inv

  -- | 'invRev' 'p' returns the inverset inverse of the permutation 'p'.
  --
  -- prop> invRev p == compRev
  invRev :: Perm -> Perm
  invRev = comp . inv

  -- | 'invComp' 'p' returns the inverse complement of the permutation 'p'.
  --
  -- >>> invComp $ mk [1,3,4,2]
  -- [3,2,4,1]
  invComp :: Perm -> Perm
  invComp = inv . comp

  -- | 'revInv' 'p' returns the reverse inverse of the permutation 'p'.
  --
  -- prop> revInv p == invComp p
  revInv :: Perm -> Perm
  revInv = invComp

  -- | 'invRevComp' 'p' returns the inverse reverse complement of permutation 'p'.
  --
  -- >>> invRevComp $ mk [1,3,4,2]
  -- [2,3,1,4
  invRevComp :: Perm -> Perm
  invRevComp = inv . rev . comp

  -- transpose :: Int -> Int -> Perm -> Perm
  -- transpose i j = mk $ L.prefix (i-1) xs ++ [xs L.!! i] ++

  -- |'extendLeft' 'p' takes a permutation 'p' of length 'n' and returns all
  -- permutations of length 'n'+1 which suffix of length 'n' is order-isomorphic
  -- to 'p'.
  --
  -- >>> extendLeft $ mk [2,1,3]
  -- [[1,3,2,4],[2,3,1,4],[3,2,1,4],[4,2,1,3]]
  extendLeft :: Perm -> [Perm]
  extendLeft p = L.map mkPermUnsafe [k : f k ys | k <- [1..len p+1]]
    where
      ys  = getList p
      f k = F.foldr (\y acc -> (if y<k then y else y+1) : acc) []

  -- |'extendRight' 'p' takes a permutation 'p' of length 'n' and returns all
  -- permutations of length 'n'+1 which prefix of length 'n' is order-isomorphic
  -- to 'p'.
  --
  -- >>> extendRight $ mk [2,1,3]
  -- [[3,2,4,1],[3,1,4,2],[2,1,4,3],[2,1,3,4]]
  extendRight :: Perm -> [Perm]
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
  grid :: Perm -> String
  grid p = aux . L.map (row . T.fst) . L.reverse . L.sortOn T.snd $ getPoints p
    where
      n      = len p
      sep    = ('+' :) $ F.concat (L.replicate n "---+") ++ "\n"
      row x  = ('|' :) $ F.concat [F.concat (L.replicate (x-1) "   |"), " o |", F.concat (L.replicate (n-x) "   |"), "\n"]
      aux ss = sep ++ L.intercalate sep ss ++ sep

  grid' :: Perm -> String
  grid' p = ""
