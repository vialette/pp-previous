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

  import qualified Data.Algorithm.PP.Combi        as PP.Combi
  import qualified Data.Algorithm.PP.Perm         as PP.Perm
  import qualified Data.Algorithm.PP.Perm.GSquare as PP.Perm.GSquare
  import qualified Data.Algorithm.PP.Utils.List   as PP.Utils.List

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
  squareRoots :: PP.Perm.Perm -> [PP.Perm.Perm]
  squareRoots = PP.Perm.GSquare.squareRoots id

  squareRootsStat ::  PP.Perm.Perm -> Int
  squareRootsStat = PP.Perm.GSquare.squareRootsStat id

  squareRootsMult ::  PP.Perm.Perm -> Int
  squareRootsMult = PP.Perm.GSquare.squareRootsMult id

  -- | 'isSquare' 'p' returns 'True' if and only if the permutation 'p' is square.
  --
  -- >>> import qualified Data.Algorithm.PP.Perm as Perm
  -- >>> import qualified Data.Algorithm.PP.Perm.Square as Square
  -- >>>
  -- >>> PP.Perm.isSquare $ Perm.mk [3,4,2,4]
  -- True
  -- >>> PP.Perm.squareRoots $ Perm.mk [3,4,2,4] -- find its square roots
  -- [[1,2]]
  -- >>> PP.Perm.isSquare $ Perm.mk [3,2,1,4]
  -- False
  -- >>> PP.Perm.squareRoots $ Perm.mk [3,2,1,4] -- find its square roots
  -- []
  isSquare :: PP.Perm.Perm -> Bool
  isSquare = PP.Perm.GSquare.isSquare id

  -- |'isSimpleSquare' 'p' returns 'True' if and only if the permutation 'p' has
  -- exactly one square root.
  --
  -- >>> import qualified Data.Algorithm.PP.Perm as Perm
  -- >>> import qualified Data.Algorithm.PP.Perm.Square as Square
  -- >>>
  isSimpleSquare :: PP.Perm.Perm -> Bool
  isSimpleSquare = PP.Perm.GSquare.isSimpleSquare id

  isKSquare :: Int -> PP.Perm.Perm -> Bool
  isKSquare = PP.Perm.GSquare.isKSquare id

  isExtremalSquare :: PP.Perm.Perm -> Bool
  isExtremalSquare p = False

  -- |'squares' 'n' returns all square permutations of length 'n'.
  squares :: Int -> [PP.Perm.Perm]
  squares = PP.Perm.GSquare.squares id

  -- |'nonSquares' 'n' returns all non-square permutations of length 'n'.
  nonSquares :: Int -> [PP.Perm.Perm]
  nonSquares = PP.Perm.GSquare.nonSquares id

  -- |'subSquares' 'p' return the longest square subpermutations of permutation 'p'.
  subSquares :: PP.Perm.Perm -> [PP.Perm.Perm]
  subSquares = PP.Perm.GSquare.subSquares id
