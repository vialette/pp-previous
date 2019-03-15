module Data.Algorithm.PP.Perm.ShuffleSquare
  (
  -- * Searching
    shuffleSquareRoots
  , shuffleSquareRootCount

    -- * Testing
  , shuffleSquare
  , simpleShuffleSquare
  , extremalShuffleSquare
  , extremalShuffleSquares
  , kShuffleSquare
  , maxShuffleSquareRootsCount
  , kShuffleSquareFree
  , shuffleSquareFree

    -- * Generating
  , shuffleSquares
  , nonShuffleSquares

    -- * Sub-permutation
  , subShuffleSquares
  -- , subShuffleSquaresStat
  -- , subShuffleSquaresStat'
  ) where

import Data.Maybe
import qualified Control.Arrow      as A
import qualified Data.Foldable      as F
import qualified Data.List          as L
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Tuple         as T

import qualified Data.Algorithm.PP.Perm                  as PP.Perm
import qualified Data.Algorithm.PP.Perm.Generator        as PP.Perm.Generator
import qualified Data.Algorithm.PP.Perm.ShuffleSquare.DB as PP.Perm.ShuffleSquare.DB
import qualified Data.Algorithm.PP.Perm.ShuffleSquareBy  as PP.Perm.ShuffleSquareBy
import qualified Data.Algorithm.PP.Utils.List            as PP.Utils.List

{- | 'shuffleSquareRoots' @p@ returns the list of all shuffle square roots of the permutation @p@
(i.e., all permutations @q@ such that the permutation @p@ is the disjoint union of two copies of @q@).

\[
\forall p \in S_{n},
\quad
\texttt{ shuffleSquareRoots } p
\;=\;
 \left\{q : p \in q \bullet q\right\}
\]

>>> shuffleSquareRoots $ mkPerm [3,1,4,2,5,6]
[[1,2,3],[2,1,3]]
>>> shuffleSquareRoots $ mkPerm [6,3,1,5,4,2]
[[3,2,1]]
>>> shuffleSquareRoots $ mkPerm [6,2,3,1,5,4]
[]
-}
shuffleSquareRoots :: PP.Perm.Perm -> [PP.Perm.Perm]
shuffleSquareRoots = PP.Perm.ShuffleSquareBy.shuffleSquareRootsBy id

{- | 'squareRootCount' @p@ returns the number of distinct shuffle square roots of the permutation @p@.

\[
\forall p \in S_{n},
\quad
\texttt{ shuffleSquareRootCount } p
\;=\;
\left|\left\{q : p \in q \bullet q\right\}\right|
\]

>>> shuffleSquareRootCount $ mkPerm [3,1,4,2,5,6]
3
>>> shuffleSquareRootCount $ mkPerm [6,3,1,5,4,2]
1
>>> shuffleSquareRootCount $ mkPerm [6,2,3,1,5,4]
0
-}
shuffleSquareRootCount ::  PP.Perm.Perm -> Int
shuffleSquareRootCount = PP.Perm.ShuffleSquareBy.shuffleSquareRootByCount id

{- | 'shuffleSquare' @p@ returns @True@ if the permutation @p@ is a square according to the shuffle operator.

\[
\forall p \in S_{2n},
\quad
\texttt{shuffleSquare} \; p
\;\Leftrightarrow\;
\exists q \in S_{n},\; p \in q \bullet q
\]

>>> shuffleSquare (mkPerm [3,4,2,4])
True
>>> shuffleSquare (mkPerm [3,2,1,4])
False
-}
shuffleSquare :: PP.Perm.Perm -> Bool
shuffleSquare = PP.Perm.ShuffleSquareBy.shuffleSquareBy id

-- |'simpleShuffleSquare' 'p' returns 'True' if the permutation 'p' has
-- exactly one shuffle square root.
--
-- \[
-- \begin{align*}
-- \forall p \in S_{n},
-- \quad
-- \texttt{ simpleShuffleSquare } p
-- &\;\Leftrightarrow\;
-- \left|\left\{q : q \in \texttt{ shuffleSquareRoots } p\right\}\right| = 1 \\
-- &\;\Leftrightarrow\;
-- \texttt{ shuffleSquareRootCount } p = 1
-- \end{align*}
-- \]
--
-- >>> simpleShuffleSquare (mkPerm [2,1,3,4])
-- True
-- >>> shuffleSquareRoots (mkPerm [2,1,3,4])
-- [[1,2]]
-- >>> simpleShuffleSquare (mkPerm [2,1,4,3])
-- False
-- >>> shuffleSquareRoots (mkPerm [2,1,4,3])
-- [[1,2],[2,1]]
-- >>> simpleShuffleSquare (mkPerm [6,2,3,1,5,4])
-- False
-- >>> shuffleSquareRoots (mkPerm [6,2,3,1,5,4])
-- []
simpleShuffleSquare :: PP.Perm.Perm -> Bool
simpleShuffleSquare = PP.Perm.ShuffleSquareBy.simpleShuffleSquareBy id

-- 'isKShuffleSquare' 'k' 'p' return 'True' if the permutation 'p' has
-- 'k' distinct shuffleSquare roots.
kShuffleSquare :: Int -> PP.Perm.Perm -> Bool
kShuffleSquare = PP.Perm.ShuffleSquareBy.kShuffleSquareBy id

-- |'maxShuffleSquareRootsCount' 'n'
maxShuffleSquareRootsCount :: Int -> Int
maxShuffleSquareRootsCount n = fromMaybe k (L.lookup n PP.Perm.ShuffleSquare.DB.assoc)
  where
    k = maximum . L.map shuffleSquareRootCount $ PP.Perm.Generator.perms n

-- | 'extremalShuffleSquare' 'p' returns 'True' if the permutation 'p'
-- has the maximum number of permutations.
--
-- \[
-- \forall p \in S_{2n},
-- \quad
-- \texttt{extremalShuffleSquare} \; p
-- \;\Leftrightarrow\;
-- \not\exists q \in S_{2n},\; \texttt{shuffleSquareRootCount}\;p < \texttt{shuffleSquareRootCount}\;q
-- \]
extremalShuffleSquare :: PP.Perm.Perm -> Bool
extremalShuffleSquare p = k == k'
  where
    n  = PP.Perm.len p
    k  = shuffleSquareRootCount p
    k' = maxShuffleSquareRootsCount n

extremalShuffleSquares :: Int -> (Int, [PP.Perm.Perm])
extremalShuffleSquares = F.foldr f (0, []) . shuffleSquares
  where
    f p (mult, acc)
      | k > mult              = (k, [p])
      | k == mult && mult > 0 = (mult, p : acc)
      | otherwise             = (mult, acc)
        where
          k = shuffleSquareRootCount p

kShuffleSquareFree :: Int -> PP.Perm.Perm -> Bool
kShuffleSquareFree = PP.Perm.ShuffleSquareBy.kShuffleSquareByFree id

-- |'shuffleSquareByFree' 'f' 'p' retusn 'True' if the permutations 'p' does not
-- contain any pattern of length at least 4 that is a shuffle square
--
-- \[
-- \forall p \in S_{2n},
-- \quad
-- \texttt{shuffleSquareFree} \; p
-- \;\Leftrightarrow\;
-- \not\exists q \in S_{2k},\;k \geq 2,\;
-- q \preceq p \;\text{and}\;\texttt{shuffleSquare}\;q
-- \]
shuffleSquareFree :: PP.Perm.Perm -> Bool
shuffleSquareFree = PP.Perm.ShuffleSquareBy.shuffleSquareByFree id

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

-- |'subShuffleSquares' 'p' return the longest shuffle square subpermutations
-- of the permutation 'p'.
--
-- >>> isShuffleSquare (mkPerm [6,7,5,4,3,2,8,1])
-- False
-- >>> subShuffleSquares (mkPerm [6,7,5,4,3,2,8,1])
-- [[4,5,3,2,6,1],[5,6,4,3,2,1],[6,5,4,3,2,1]]
-- >>> all isShuffleSquare . subShuffleSquares $ mkPerm [6,7,5,4,3,2,8,1]
-- True
-- >>> isShuffleSquare (mkPerm [3,6,2,5,4,7,1,8])
-- True
-- >>> subShuffleSquares (mkPerm [3,6,2,5,4,7,1,8])
-- [[3,6,2,5,4,7,1,8]]
subShuffleSquares :: PP.Perm.Perm -> [PP.Perm.Perm]
subShuffleSquares = PP.Perm.ShuffleSquareBy.subShuffleSquaresBy id
