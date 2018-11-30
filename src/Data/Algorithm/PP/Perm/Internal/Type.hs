module Data.Algorithm.PP.Perm.Internal.Type
(
  -- * Type
  P(..)
, Perm
, Patt
, FPerm
)
where

  import qualified Data.Foldable as F
  import qualified Data.List     as L

  import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point

  -- |'Seq' type
  newtype P a = P { getPoints :: [PP.Geometry.Point.Point] } deriving (Eq, Ord)

  -- |
  instance Show (P a) where
    show = show . L.map PP.Geometry.Point.getY . getPoints

  -- |'Span' type
  data Span

  -- |'Sub' type
  data Sub

  -- |'Perm' type
  type Perm = P Span

  -- |'Patt' type
  type Patt = P Sub

  type FPerm = Perm -> Perm
