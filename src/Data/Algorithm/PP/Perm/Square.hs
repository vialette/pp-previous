module Data.Algorithm.PP.Perm.Square
(
  -- * Testing
  isSquare
, isSimpleSquare
, isExtremalSquare
, isKSquare

  -- * Searching
, squareRoots
, squareRootsStat
, squareRootsMult

  -- * Generating
, squares
, nonSquares

  -- * Subpermutation
, subSquares
-- , subSquaresStat
-- , subSquaresStat'
)
where

  import qualified Control.Arrow  as A
  import qualified Data.List      as L
  import qualified Data.Tuple     as T

  import qualified Data.Algorithm.PP.Combi      as PP.Combi
  import qualified Data.Algorithm.PP.Perm       as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List

  squareRootsT :: [PP.Perm.T] -> [[PP.Perm.T]]
  squareRootsT = PP.Utils.List.uniq . L.map h . L.filter g . L.map f . PP.Combi.balPartitions
    where
      f = reduce A.*** reduce
      g = T.uncurry (==)
      h = T.fst

  squareRoots :: PP.Perm.Perm -> [PP.Perm.Perm]
  squareRoots = L.map P.Perm.fromList . squareRootsT . PP.Perm.toList

  squareRootsStat ::  PP.Perm.Perm -> Int
  squareRootsStat = L.length . squareRoots

  squareRootsMult ::  PP.Perm.Perm -> Int
  squareRootsMult = squareRootsStat

  isSquare :: PP.Perm.Perm -> Bool
  isSquare = not . L.null . squareRoots

  isSimpleSquare :: PP.Perm.Perm -> Bool
  isSimpleSquare = go . squareRoots
    where
      go [p] = True
      go _   = False

  isKSquare :: Int -> PP.Perm.Perm -> Bool
  isKSquare k = (==) k . L.length . squareRoots

  isExtremalSquare :: PP.Perm.Perm -> Bool
  isExtremalSquare = False

  -- |'squares' 'n' returns all square permutations of length 'n'.
  squares :: Int -> [PP.Perm.Perm]
  squares n
    | odd n     = []
    | otherwise = L.filter isSquare . PP.Perm.pems

  -- |'nonSquares' 'n' returns all non-square permutations of length 'n'.
  nonSquares :: Int -> [PP.Perm.Perm]
  nonSquares n
    | odd n     = PP.Perm.perms n
    | otherwise = L.filter (not . isSquare) . PP.Perm.pems

  -- |'subSquares' 'p' return the longest square subpermutations of permutation 'p'.
  subSquares :: Perm -> [Perm]
  subSquares p = go $ PP.Perm.len p
    where
      go k
        | odd k     = go (k-1)
        | otherwise = if L.null sqs then go (k-2) else sqs
        where
          sqs = L.filter isSquare (sub k p)
