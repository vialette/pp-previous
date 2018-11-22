<!-- python -m readme2tex --usepackage "tikz" --usepackage "xcolor" --output README.md --readme  READOTHER.md --nocdn --pngtrick -->

# PPattern - Permutation Pattern

A permutation of length $n$ is a bijective mapping
$\tau : [n] \to [n]$; one way to represent it is as the
sequence of numbers $\tau[1] \tau[2] \ldots \tau[n]$.
A permutation $\tau$ *contains* permutation $\pi$ if $\pi$
has a (not necessarily consecutive) subsequence where the relative
ordering of the elements is the same as in $\tau$.
In this case, $\pi$ is a *subpattern* of $\tau$;
otherwise, $\tau$ avoids $\pi$.
For example, $3215674$ contains the pattern $132$, since the subsequence
$154$ is ordered the same way as $132$.
On the other hand, the permutation avoids $4321$: it does
not contain a descending subsequence of 4 elements.

Given permutation $\pi$ and $\tau$, the *Permutation Pattern* problem
is to decide if $\tau$ contains $\pi$.
The *Permutation Pattern* problem is NP-complete.
It can be solved by brute force in time $O(n^k)$, where
$n = |\tau|$ and $k = |\pi|$.
This has been improved to $O(n^{0.47k + o(k)})$
by Ahal and Rabinovich.
Guillemot and Marx proved that the *Permutation Pattern* problem
can be solved in time $2^{O(k^2 \log(k))}}$
(i.e., the *Permutation Pattern* problem is fixed-parameter tractable
parameterized by the size of the pattern).

## Permutations

### Implementation

Permutations are implemented as lists of points with increasing x-coordinates.

```haskell
-- Define in Data.Algorithm.PPattern.Geometry.Point.hs
newtype Point = Point (Int, Int) deriving (Show, Eq, Ord)

-- Define in Data.Algorithm.PPattern.Perm.hs
newtype Perm = Perm { getList :: [Point] } deriving (Eq, Ord)
```

The function `Data.Algorithm.PPattern.Perm.mk :: (Foldable t, Ord a) => t a -> Perm`
is devoted to creating permutations from foldable objects.

```haskell
λ: Perm.mk [2,1,3]
[2,1,3]
λ: Perm.mk "bac"
[2,1,3]
λ: Perm.mk ["tomorrow", "today", "yesterday"]
[2,1,3]
λ: Perm.mk "bac" == Perm.mk ["tomorrow", "today", "yesterday"]
True
```

### Basic manipulation

```haskell
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: let p = Perm.mk "acedb"
λ: p
[1,3,5,4,2]
λ: Perm.size p
5
λ: Perm.points p
[Point (1,1),Point (2,3),Point (3,5),Point (4,4),Point (5,2)]
λ: Perm.xCoords p
[1,2,3,4,5]
λ: Perm.yCoords p
[1,3,5,4,2]
λ: mapM_ print [Perm.pointAtXCoord x p | x <- [0..6]]
Nothing
Just (Point (1,1))
Just (Point (2,3))
Just (Point (3,5))
Just (Point (4,4))
Just (Point (5,2))
Nothing
λ: mapM_ print [Perm.pointAtYCoord y p | y <- [0..6]]
Nothing
Just (Point (1,1))
Just (Point (5,2))
Just (Point (2,3))
Just (Point (4,4))
Just (Point (3,5))
Nothing
```

Notice that `Data.Algorithm.PPattern.Perm.xCoords` and
`Data.Algorithm.PPattern.Perm.yCoords` are reduced forms for:

```haskell
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: import qualified Data.Algorithm.PPattern.Geometry.Point as Point
λ: fmap Point.xCoord (Perm.points p)
[1,2,3,4,5]
λ: fmap Point.yCoord (Perm.points p)
[1,3,5,4,2]
```

As you might have guessed, `show` for permutations reduces to `show . yCoords`:

```haskell
instance Show Perm where
  show = show . yCoords
```

### Ties

Ties are allowed and are resolved according to the left-to-right order.

```haskell
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: Perm.mk "acb"
[1,3,2]
λ: Perm.mk "acbacb"
[1,5,3,2,6,4]
λ: Perm.mk "acbacbacb"
[1,7,4,2,8,5,3,9,6]
```

Use `Data.Algorithm.PPattern.Perm.mkSafe` to forbid ties.

```haskell
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: Perm.mkSafe "acb"
Just [1,3,2]
λ: Perm.mkSafe "acbacb"
Nothing
```

### Basic operations

The *reverse* of a permutation $\sigma = \sigma_1 \sigma_1 \ldots \sigma_n$
is the permutation $\sigma_n \sigma_{n-1} \ldots \sigma_1$.
The *complement* of $\sigma$ is the permutation
$\beta_1 \beta_2 \ldots \beta_n$, where
$\beta_i = n+1-\sigma_i$.
That is, the complement substitutes the largest element of a permutation
by the smallest one, the next largest element by the next smallest element, etc.
The *inverse* of $\sigma$ is the permutation where the $\sigma_i$-th position
is occupied by $i$.

```haskell
λ: import qualified Data.Algorithm.PPattern.APerm as APerm
λ: import qualified Data.Algorithm.PPattern.Perm.Operation as Perm.Operation
λ: let p = Perm.mk [1,4,2,5,3]
λ: Perm.Operation.reversal p
[3,5,2,4,1]
λ: Perm.Operation.complement p
[5,2,4,1,3]
λ: Perm.Operation.reversalComplement p
[3,1,4,2,5]
λ: (Perm.Operation.reversal . Perm.Operation.complement) p
[3,1,4,2,5]
λ: (Perm.Operation.complement . Perm.Operation.reversal) p
[3,1,4,2,5]
λ: Perm.Operation.inverse p
[1,3,5,2,4]
```

### Basic statistics

An *ascent* (resp. *descent*) in a permutation is an entry followed by a
larger (resp. smaller) entry.
An entry of a permutation which is smaller (resp. larger) than
all the entries that precede it is called a *left-to-right minimum*
(resp. *left-to-right maximum*).
An entry of a permutation which is smaller (resp. larger) than
all the entries that follow it is called a *right-to-left minimum*
(resp. *right-to-left maximum*).
A *valley* (resp. *peak*) in a permutation is an entry that is smaller (resp. larger)
than both of its neighbors.

```haskell
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: import qualified Data.Algorithm.PPattern.Perm.Statistics as Perm.Statistics
λ: let p = Perm.mk [7,5,3,8,2,1,4,9,6]
λ: Perm.Statistics.leftToRightMinima p
[Point (1,7),Point (2,5),Point (3,3),Point (5,2),Point (6,1)]
λ: Perm.Statistics.leftToRightMaxima p
[7,8,9]
λ: Perm.Statistics.leftToRightMaxima p
[Point (1,7),Point (4,8),Point (8,9)]
λ: Perm.Statistics.rightToLeftMaxima p
[Point (8,9),Point (9,6)]
λ: Perm.Statistics.ascents p
[Point (3,3),Point (6,1),Point (7,4)]
λ: Perm.Statistics.doubleAscents p
[Point (6,1)]
λ: Perm.Statistics.descents p
[Point (1,7),Point (2,5),Point (4,8),Point (5,2),Point (8,9)]
λ: Perm.Statistics.doubleDescents p
[Point (1,7),Point (4,8)]
λ: Perm.Statistics.peaks p
[Point (4,8),Point (8,9)]
λ: Perm.Statistics.valleys p
[Point (3,3),Point (6,1)]
```

## Pattern matching

### Basic pattern matching

The `Data.Algorithm.PPattern.search` function is for searching the occurrence of
a pattern (short permutation) in a larger permutatins.
The result (in case of succeed) is given in the form of a point to point mapping.
The `Data.Algorithm.PPattern.occursIn`,
`Data.Algorithm.PPattern.avoids` and
`Data.Algorithm.PPattern.contains` functions discard the solution mapping and
return a boolean.

+ Positive search:

```haskell
λ: import qualified Data.Algorithm.PPattern as PPattern
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: let p = Perm.mk [3,2,4,1]
λ: let q = Perm.mk [5,4,3,6,2,1]
λ: PPattern.search p q
Just Point (1,3) -> Point (2,4), Point (2,2) -> Point (3,3), Point (3,4) -> Point (4,6), Point (4,1) -> Point (6,1)
λ: p `PPattern.occursIn` q
True
λ: q `PPattern.contains` p
True
λ: q `PPattern.avoids` p
False
```

+ Negative search:

```haskell
λ: import qualified Data.Algorithm.PPattern as PPattern
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: let p = Perm.mk [1,2,3]
λ: let q = Perm.mk [5,4,3,6,2,1]
λ: PPattern.search p q
Nothing
λ: p `PPattern.occursIn` q
False
λ: q `PPattern.contains` p
True
λ: q `PPattern.avoids` p
True

```

### Occurrence

The `Data.Algorithm.PPattern.search` function has type
```haskell
import qualified Data.Algorithm.PPattern.Perm as Perm
import qualified Data.Algorithm.PPattern.Search.Occurrence as Occurrence
search :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
```

The `Data.Algorithm.PPattern.Occurrence` module provides several functions
for querying `Data.Algorithm.PPattern.Occurrence.Occurrence` type variables.

```haskell
λ: import qualified Data.Maybe as Maybe
λ: import qualified Data.Algorithm.PPattern as PPattern
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: import qualified Data.Algorithm.PPattern.Search.Occurrence as Occurrence
λ: import qualified Data.Algorithm.PPattern.Geometry.Point as Point
λ: let q = Perm.mk [5,4,3,6,2,1]
λ: let p = Perm.mk [3,2,4,1]
λ: let o = PPattern.search p q
λ: Occurrence.size (Maybe.fromJust o)
4
λ: Occurrence.pattern (Maybe.fromJust o)
[3,2,4,1]
λ: Occurrence.patternPoints (Maybe.fromJust o)
[Point (1,3),Point (2,2),Point (3,4),Point (4,1)]
λ: Occurrence.target (Maybe.fromJust o)
[3,2,4,1]
λ: Occurrence.targetPoints (Maybe.fromJust o)
[Point (2,4),Point (3,3),Point (4,6),Point (6,1)]
λ: fmap Point.xCoord (Occurrence.targetPoints (fromJust o))
[2,3,4,6]
λ: fmap Point.yCoord (Occurrence.targetPoints (fromJust o))
[4,3,6,1]
```

Of course, the following assertions always hold.

```haskell
λ: Maybe.isNothing o || p == Occurrence.pattern (Maybe.fromJust o)
True
λ: Maybe.isNothing o || p == Occurrence.target (Maybe.fromJust o)
True
```

### Resolving conflicts

Conflict resolution is a key part of the approach.
Given two permutations, $\pi$ of $[m]$ and $\tau$ of $[n]$, a *pre-embedding*
of $\pi$ into $\tau$ is any function $e: [m] \to [n]$.
A pair $(i, j)$ with $i < j$ is a

+ *horizontal* conflict for $e$ if $e(i) > e(j)$,

+ *vertical* conflict for $e$ if $\pi[i] < \pi[j]$ and $\tau[e(i)] > \tau[e(j)]$,
or $\pi[i] > \pi[j]$ and $\tau[e(i)] <\tau[e(j)]$.

The algorithm used by `PPattern` is to recursively find and resolve conflicts.
If a conflict cannot be resolved then there is no occurrence of the pattern in
the permutation.
Several conflict selection strategies are conceivable.
The following conflict selection strategies are implemented within `PPattern`.

+ *leftmost conflict first* (`Data.Algorithm.PPattern.Search.ConflictSelection.LeftmostConflictFirst`):
Resolve - if possible - any leftmost conflict first
(this conflict is either a horizontal or a vertical conflict).

+ *leftmost horizontal conflict* (`Data.Algorithm.PPattern.Search.ConflictSelection.LeftmostHorizontalConflictFirst`):
Resolve - if possible - the leftmost horizontal conflict first.
If such a conflict does not not exist,
resolve - if possible -  the leftmost vertical conflict first.

+ *leftmost vertical conflict* (`Data.Algorithm.PPattern.Search.ConflictSelection.LeftmostVerticalConflictFirst`):
Resolve - if possible - the leftmost vertical conflict first.
If such a conflict does not not exist,
resolve - if possible - the leftmost horizontal conflict first.

+ *rightmost conflict first* (`Data.Algorithm.PPattern.Search.ConflictSelection.RightmostConflictFirst`):
Resolve - if possible - any rightmost conflict first
(this conflict is either a horizontal or a vertical conflict).

+ *rightmost horizontal conflict* (`Data.Algorithm.PPattern.Search.ConflictSelection.RightmostHorizontalConflictFirst`):
Resolve - if possible - the rightmost horizontal conflict first.
If such a conflict does not not exist,
resolve - if possible - the rightmost vertical conflict first.

+ *rightmost vertical conflict* (`Data.Algorithm.PPattern.Search.ConflictSelection.RighmostVerticalConflictFirst`):
Resolve - if possible - the rightmost vertical conflict first.
If such a conflict does not not exist,
resolve - if possible - the rightmost horizontal conflict first.

As of version 0.1.0.0 of `PPattern`,
the `Data.Algorithm.PPattern.search` function uses a default
leftmost horizontal conflict first resolution algorithm.
The `Data.Algorithm.PPattern` module offers the following search functions for using
a specific conflict resolution algorithm.

```haskell
import qualified Data.Algorithm.PPattern.Perm as Perm
import qualified Data.Algorithm.PPattern.Search.Occurrence as Occurrence

searchLeftmostConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
searchLeftmostHorizontalConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
searchLeftmostVerticalConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
searchRightmostConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
searchRightmostHorizontalConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
searchRightmostVerticalConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
```

The `Data.Algorithm.PPattern.searchWithConflictSelectionStrategy` allows for
new implemented conflict selection strategies.
