module Data.Algorithm.PP.Perm.Square
(
  -- * Testing
  isSquare
, isSimpleSquare
, isExtremalSquare
, isKSquare
, maxSquareRootsMult

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

  import Data.Maybe
  import qualified Control.Arrow      as A
  import qualified Data.List          as L
  import qualified Data.IntMap.Strict as IntMap
  import qualified Data.Tuple         as T

  import qualified Data.Algorithm.PP.Combi        as PP.Combi
  import qualified Data.Algorithm.PP.Perm         as PP.Perm
  import qualified Data.Algorithm.PP.Perm.GSquare as PP.Perm.GSquare
  import qualified Data.Algorithm.PP.Utils.List   as PP.Utils.List

  -- |'squareRoots' 'p' return the list of all square root permutations of 'p'
  -- (i.e., all permutation 'q' such that permutation 'p' is the disjoint union
  -- of 'q' and 'q').
  --
  -- >>> import qualified Data.Algorithm.PP.Perm as Perm
  -- >>>
  -- >>> SquaresquareRoots $ Perm.mk [3,1,4,2,5,6]
  -- [[1,2,3],[2,1,3]]
  -- >>> SquaresquareRoots $ Perm.mk [6,3,1,5,4,2]
  -- [[3,2,1]]
  -- >>> SquaresquareRoots $ Perm.mk [6,2,3,1,5,4]
  -- []
  squareRoots :: PP.Perm.Perm -> [PP.Perm.Perm]
  squareRoots = PP.Perm.GSquare.squareRoots id

  -- |'squareRootsStat' 'p' return the number of distinct square roots of the
  -- permutation 'p'.
  --
  -- >>> squareRootsStat $ Perm.mk [3,1,4,2,5,6]
  -- 3
  -- >>> SquaresquareRoots $ Perm.mk [3,1,4,2,5,6]
  -- [[1,2,3],[2,1,3]]
  -- >>> squareRootsStat $ Perm.mk [6,3,1,5,4,2]
  -- 1
  -- >>> SquaresquareRoots $ Perm.mk [6,3,1,5,4,2]
  -- [[3,2,1]]
  -- >>> squareRootsStat $ Perm.mk [6,2,3,1,5,4]
  -- 0
  -- >>> SquaresquareRoots $ Perm.mk [6,2,3,1,5,4]
  -- []
  squareRootsStat ::  PP.Perm.Perm -> Int
  squareRootsStat = PP.Perm.GSquare.squareRootsStat id

  -- | Alias for 'squareRootsStat'.
  squareRootsMult ::  PP.Perm.Perm -> Int
  squareRootsMult = PP.Perm.GSquare.squareRootsMult id

  -- | 'isSquare' 'p' returns 'True' if and only if the permutation 'p' is square.
  --
  -- >>> import qualified Data.Algorithm.PP.Perm as Perm
  -- >>>
  -- >>> isSquare $ Perm.mk [3,4,2,4]
  -- True
  -- >>> squareRoots $ Perm.mk [3,4,2,4] -- find its square roots
  -- [[1,2]]
  -- >>> isSquare $ Perm.mk [3,2,1,4]
  -- False
  -- >>> squareRoots $ Perm.mk [3,2,1,4] -- find its square roots
  -- []
  isSquare :: PP.Perm.Perm -> Bool
  isSquare = PP.Perm.GSquare.isSquare id

  -- |'isSimpleSquare' 'p' returns 'True' if and only if the permutation 'p' has
  -- exactly one square root.
  --
  -- >>> import qualified Data.Algorithm.PP.Perm as Perm
  -- >>>
  -- >>> isSimpleSquare $ Perm.mk [2,1,3,4]
  -- True
  -- >>> squareRoots $ Perm.mk [2,1,3,4]
  -- [[1,2]]
  -- >>> isSimpleSquare $ Perm.mk [2,1,4,3]
  -- False
  -- >>> squareRoots $ Perm.mk [2,1,4,3]
  -- [[1,2],[2,1]]
  -- >>> isSimpleSquare $ Perm.mk [6,2,3,1,5,4]
  -- False
  -- >>> SquaresquareRoots $ Perm.mk [6,2,3,1,5,4]
  -- []
  isSimpleSquare :: PP.Perm.Perm -> Bool
  isSimpleSquare = PP.Perm.GSquare.isSimpleSquare id

  -- 'isKSquare' 'k' 'p' return 'True' if and only if the permutation 'p' has
  -- 'k' distinct square roots.
  isKSquare :: Int -> PP.Perm.Perm -> Bool
  isKSquare = PP.Perm.GSquare.isKSquare id

  -- | 'extremalSquaresStat' 'n' returns the maximum number of distinct square roots
  -- a permutation of length 'n' can have.
  maxSquareRootsMult :: Int -> Int
  maxSquareRootsMult n = fromMaybe k (IntMap.lookup n m)
    where
      database = [(2, 1), (4, 2), (6, 3), (8, 7), (10, 10)]
      m        = IntMap.fromList database
      k        = maximum . L.map squareRootsStat $ PP.Perm.perms n

  -- | 'isExtremalSquare' 'p' returns 'True' if and only if the permutation 'p'
  -- has the maximum number of permutations.
  isExtremalSquare :: PP.Perm.Perm -> Bool
  isExtremalSquare p = k == k'
    where
      n  = PP.Perm.len p
      k  = squareRootsStat p
      k' = maxSquareRootsMult n

  -- |'squares' 'n' returns all square permutations of length 'n'.
  --
  -- >>> squares 4
  -- [[1,2,3,4],[2,1,3,4],[2,3,1,4],[3,1,2,4],[1,3,2,4],[4,3,2,1],[3,4,2,1],[3,2,4,1],[4,2,3,1],[2,4,3,1],[1,4,2,3],[1,2,4,3],[4,2,1,3],[2,4,1,3],
  --  [2,1,4,3],[4,1,3,2],[1,3,4,2],[4,3,1,2],[3,4,1,2],[3,1,4,2]]
  squares :: Int -> [PP.Perm.Perm]
  squares = PP.Perm.GSquare.squares id

  -- |'nonSquares' 'n' returns all non-square permutations of length 'n'.
  --
  -- >>> nonSquares 4
  -- [[3,2,1,4],[2,3,4,1],[4,1,2,3],[1,4,3,2]]
  nonSquares :: Int -> [PP.Perm.Perm]
  nonSquares = PP.Perm.GSquare.nonSquares id

  -- |'subSquares' 'p' return the longest square subpermutations of permutation 'p'.
  --
  -- >>> import qualified Data.Algorithm.PP.Perm as Perm
  -- >>>
  -- >>> isSquare $ Perm.mk [6,7,5,4,3,2,8,1]
  -- False
  -- >>> subSquares $ Perm.mk [6,7,5,4,3,2,8,1]
  -- [[4,5,3,2,6,1],[5,6,4,3,2,1],[6,5,4,3,2,1]]
  -- >>> all isSquare . subSquares $ Perm.mk [6,7,5,4,3,2,8,1]
  -- True
  -- >>> isSquare $ Perm.mk [3,6,2,5,4,7,1,8]
  -- True
  -- >>> subSquares $ Perm.mk [3,6,2,5,4,7,1,8]
  -- [[3,6,2,5,4,7,1,8]]
  subSquares :: PP.Perm.Perm -> [PP.Perm.Perm]
  subSquares = PP.Perm.GSquare.subSquares id
