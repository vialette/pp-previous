module Data.Algorithm.PP.Perm.ShuffleSquare
(
-- * Searching
  shuffleSquareRoots
, shuffleSquareRootsStat
, shuffleSquareRootsMult

  -- * Testing
, isShuffleSquare
, isSimpleShuffleSquare
, isExtremalShuffleSquare
, isKShuffleSquare
, maxShuffleSquareRootsMult

  -- * Generating
, shuffleSquares
, nonShuffleSquares

  -- * Sub-permutation
, subShuffleSquares
-- , subShuffleSquaresStat
-- , subShuffleSquaresStat'
)
where

  import Data.Maybe
  import qualified Control.Arrow      as A
  import qualified Data.List          as L
  import qualified Data.IntMap.Strict as IntMap
  import qualified Data.Tuple         as T

  import qualified Data.Algorithm.PP.Combi                as PP.Combi
  import qualified Data.Algorithm.PP.Perm                 as PP.Perm
  import qualified Data.Algorithm.PP.Perm.ShuffleSquareBy as PP.Perm.ShuffleSquareBy
  import qualified Data.Algorithm.PP.Utils.List           as PP.Utils.List


  -- |'shuffleSquareRoots' 'p' return the list of all shuffleSquare root permutations of 'p'
  -- (i.e., all permutation 'q' such that permutation 'p' is the disjoint union
  -- of 'q' and 'q').
  --
  -- >>> import qualified Data.Algorithm.PP.Perm as Perm
  -- >>>
  -- >>> shuffleSquareRoots $ Perm.mk [3,1,4,2,5,6]
  -- [[1,2,3],[2,1,3]]
  -- >>> shuffleSquareRoots $ Perm.mk [6,3,1,5,4,2]
  -- [[3,2,1]]
  -- >>> shuffleSquareRoots $ Perm.mk [6,2,3,1,5,4]
  -- []
  shuffleSquareRoots :: PP.Perm.Perm -> [PP.Perm.Perm]
  shuffleSquareRoots = PP.Perm.ShuffleSquareBy.shuffleSquareRootsBy id

  -- |'squareRootsStat' 'p' return the number of distinct shuffleSquare roots of the
  -- permutation 'p'.
  --
  -- >>> shuffleSquareRootsStat $ Perm.mk [3,1,4,2,5,6]
  -- 3
  -- >>> shuffleSquareRoots $ Perm.mk [3,1,4,2,5,6]
  -- [[1,2,3],[2,1,3]]
  -- >>> shuffleSquareRootsStat $ Perm.mk [6,3,1,5,4,2]
  -- 1
  -- >>> shuffleSquareRoots $ Perm.mk [6,3,1,5,4,2]
  -- [[3,2,1]]
  -- >>> shuffleSquareRootsStat $ Perm.mk [6,2,3,1,5,4]
  -- 0
  -- >>> shuffleSquareRoots $ Perm.mk [6,2,3,1,5,4]
  -- []
  shuffleSquareRootsStat ::  PP.Perm.Perm -> Int
  shuffleSquareRootsStat = PP.Perm.ShuffleSquareBy.shuffleSquareRootsByStat id

  -- | Alias for 'squareRootsStat'.
  shuffleSquareRootsMult ::  PP.Perm.Perm -> Int
  shuffleSquareRootsMult = PP.Perm.ShuffleSquareBy.shuffleSquareRootsByMult id

  -- | 'isShuffleSquare' 'p' returns 'True' if and only if the permutation 'p' is shuffleSquare.
  --
  -- >>> import qualified Data.Algorithm.PP.Perm as Perm
  -- >>>
  -- >>> isShuffleSquare $ Perm.mk [3,4,2,4]
  -- True
  -- >>> shuffleSquareRoots $ Perm.mk [3,4,2,4] -- find its shuffleSquare roots
  -- [[1,2]]
  -- >>> isShuffleSquare $ Perm.mk [3,2,1,4]
  -- False
  -- >>> shuffleSquareRoots $ Perm.mk [3,2,1,4] -- find its shuffleSquare roots
  -- []
  isShuffleSquare :: PP.Perm.Perm -> Bool
  isShuffleSquare = PP.Perm.ShuffleSquareBy.isShuffleSquareBy id

  -- |'isSimpleShuffleSquare' 'p' returns 'True' if and only if the permutation 'p' has
  -- exactly one shuffleSquare root.
  --
  -- >>> import qualified Data.Algorithm.PP.Perm as Perm
  -- >>>
  -- >>> isSimpleShuffleSquare $ Perm.mk [2,1,3,4]
  -- True
  -- >>> shuffleSquareRoots $ Perm.mk [2,1,3,4]
  -- [[1,2]]
  -- >>> isSimpleShuffleSquare $ Perm.mk [2,1,4,3]
  -- False
  -- >>> shuffleSquareRoots $ Perm.mk [2,1,4,3]
  -- [[1,2],[2,1]]
  -- >>> isSimpleShuffleSquare $ Perm.mk [6,2,3,1,5,4]
  -- False
  -- >>> shuffleSquareRoots $ Perm.mk [6,2,3,1,5,4]
  -- []
  isSimpleShuffleSquare :: PP.Perm.Perm -> Bool
  isSimpleShuffleSquare = PP.Perm.ShuffleSquareBy.isSimpleShuffleSquareBy id

  -- 'isKShuffleSquare' 'k' 'p' return 'True' if and only if the permutation 'p' has
  -- 'k' distinct shuffleSquare roots.
  isKShuffleSquare :: Int -> PP.Perm.Perm -> Bool
  isKShuffleSquare = PP.Perm.ShuffleSquareBy.isKShuffleSquareBy id

  maxShuffleSquareRootsMult :: Int -> Int
  maxShuffleSquareRootsMult n = fromMaybe k (IntMap.lookup n m)
    where
      database = [(2, 1), (4, 2), (6, 3), (8, 7), (10, 10)]
      m        = IntMap.fromList database
      k        = maximum . L.map shuffleSquareRootsStat $ PP.Perm.perms n

  -- | 'isExtremalShuffleSquare' 'p' returns 'True' if and only if the permutation 'p'
  -- has the maximum number of permutations.
  isExtremalShuffleSquare :: PP.Perm.Perm -> Bool
  isExtremalShuffleSquare p = k == k'
    where
      n  = PP.Perm.len p
      k  = shuffleSquareRootsStat p
      k' = maxShuffleSquareRootsMult n

  -- |'shuffleSquares' 'n' returns all shuffleSquare permutations of length 'n'.
  --
  -- >>> shuffleSquares 4
  -- [[1,2,3,4],[2,1,3,4],[2,3,1,4],[3,1,2,4],[1,3,2,4],[4,3,2,1],[3,4,2,1],[3,2,4,1],[4,2,3,1],[2,4,3,1],[1,4,2,3],[1,2,4,3],[4,2,1,3],[2,4,1,3],
  --  [2,1,4,3],[4,1,3,2],[1,3,4,2],[4,3,1,2],[3,4,1,2],[3,1,4,2]]
  shuffleSquares :: Int -> [PP.Perm.Perm]
  shuffleSquares = PP.Perm.ShuffleSquareBy.shuffleSquaresBy id

  -- |'nonShuffleSquares' 'n' returns all non-square permutations of length 'n'.
  --
  -- >>> nonShuffleSquares 4
  -- [[3,2,1,4],[2,3,4,1],[4,1,2,3],[1,4,3,2]]
  nonShuffleSquares :: Int -> [PP.Perm.Perm]
  nonShuffleSquares = PP.Perm.ShuffleSquareBy.nonShuffleSquaresBy id

  -- |'subShuffleSquares' 'p' return the longest shuffleSquare subpermutations of permutation 'p'.
  --
  -- >>> import qualified Data.Algorithm.PP.Perm as Perm
  -- >>>
  -- >>> isShuffleSquare $ Perm.mk [6,7,5,4,3,2,8,1]
  -- False
  -- >>> subShuffleSquares $ Perm.mk [6,7,5,4,3,2,8,1]
  -- [[4,5,3,2,6,1],[5,6,4,3,2,1],[6,5,4,3,2,1]]
  -- >>> all isShuffleSquare . subShuffleSquares $ Perm.mk [6,7,5,4,3,2,8,1]
  -- True
  -- >>> isShuffleSquare $ Perm.mk [3,6,2,5,4,7,1,8]
  -- True
  -- >>> subShuffleSquares $ Perm.mk [3,6,2,5,4,7,1,8]
  -- [[3,6,2,5,4,7,1,8]]
  subShuffleSquares :: PP.Perm.Perm -> [PP.Perm.Perm]
  subShuffleSquares = PP.Perm.ShuffleSquareBy.subShuffleSquaresBy id
