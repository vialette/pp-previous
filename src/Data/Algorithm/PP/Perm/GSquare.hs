module Data.Algorithm.PP.Perm.GSquare
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

  -- |'squareRoots' 'p' return the list of all square root permutations of 'p'
  -- (i.e., all permutation 'q' such that permutation 'p' is the disjoint union
  -- of 'q' and 'q').
  --
  -- >>> import qualified Data.Algorithm.PP.Perm as Perm
  -- >>> import qualified Data.Algorithm.PP.Perm.Square as Square
  -- >>>
  -- >>> SquaresquareRoots (Perm.mk [3,1,4,2,5,6])
  -- [[1,2,3],[2,1,3]]
  -- >>> SquaresquareRoots (Perm.mk [6,3,1,5,4,2])
  -- [[3,2,1]]
  -- >>> SquaresquareRoots (Perm.mk [6,2,3,1,5,4])
  -- []
  squareRoots :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> [PP.Perm.Perm]
  squareRoots f = PP.Utils.List.uniq . L.map proj1 . L.filter test . L.map trans . PP.Combi.balPartitions . PP.Perm.toList
    where
      trans = PP.Perm.mk A.*** (f . PP.Perm.mk)
      test  = T.uncurry (==)
      proj1 = T.fst

  squareRootsStat :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Int
  squareRootsStat f = L.length . squareRoots f

  squareRootsMult :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Int
  squareRootsMult = squareRootsStat

  isSquare :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Bool
  isSquare f = not . L.null . squareRoots f

  isSimpleSquare :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Bool
  isSimpleSquare f = go . squareRoots f
    where
      go [p] = True
      go _   = False

  isKSquare :: (PP.Perm.Perm -> PP.Perm.Perm) -> Int -> PP.Perm.Perm -> Bool
  isKSquare f k = (==) k . L.length . squareRoots f

  isExtremalSquare :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> Bool
  isExtremalSquare f p = False

  -- |'squares' 'n' returns all square permutations of length 'n'.
  squares :: (PP.Perm.Perm -> PP.Perm.Perm) -> Int -> [PP.Perm.Perm]
  squares f n
    | odd n     = []
    | otherwise = L.filter (isSquare f) $ PP.Perm.perms n

  -- |'nonSquares' 'n' returns all non-square permutations of length 'n'.
  nonSquares :: (PP.Perm.Perm -> PP.Perm.Perm) -> Int -> [PP.Perm.Perm]
  nonSquares f n
    | odd n     = PP.Perm.perms n
    | otherwise = L.filter (not . isSquare f) $ PP.Perm.perms n

  -- |'subSquares' 'p' return the longest square subpermutations of permutation 'p'.
  subSquares :: (PP.Perm.Perm -> PP.Perm.Perm) -> PP.Perm.Perm -> [PP.Perm.Perm]
  subSquares f p = go $ PP.Perm.len p
    where
      go k
        | odd k     = go (k-1)
        | otherwise = if L.null sqs then go (k-2) else sqs
        where
          sqs  = L.filter (isSquare f) $ PP.Perm.patterns k p
