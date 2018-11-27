module Data.Algorithm.PP.Geometry.Point
(
  -- * Type
  X
, Y
, Point(..)

  -- * Constructing
, mk
, mks
, mksX
, mksY
, symm

  -- * Comparing
, (@<-)
, (@>-)
, compX
, (@<|)
, (@>|)
, compY
, domBy
, aboveOf
, belowOf
, leftOf
, rightOf

  -- * Testing
, diag
, aboveDiag
, belowDiag


  -- * Accessing
, getX
, getY
)
where

  import qualified Data.List as L

  -- |x-coordinate type.
  type X = Int

  -- |y-coordinate type.
  type Y = Int

  -- |2D point type.
  newtype Point = Point { getCoordinates :: (X, Y) } deriving (Eq, Ord)

  instance Show Point  where
    show Point { getCoordinates = (x,y) } = "(" ++ show x ++ "," ++ show y ++ ")"

  -- |'mk' 'x' 'y' makes a point with given 'x' and 'y' coordinates.
  mk :: X -> Y -> Point
  mk x y = Point { getCoordinates = (x, y) }

  -- |'mks' 'xs' 'ys'
  mks :: [X] -> [Y] -> [Point]
  mks = zipWith mk

  -- |'mksX' 'xs'
  mksX :: [X] -> [Point]
  mksX xs = zipWith mk xs [1..]

  -- |'mksY' 'ys'
  mksY :: [Y] -> [Point]
  mksY = zipWith mk [1..]

  -- |'symm' 'p'
  symm :: Point -> Point
  symm Point { getCoordinates = (x, y) } = Point { getCoordinates = (y, x) }

  -- |'diag' 'p' return 'True' if 'x' == 'y', where 'x' and 'y' are the two
  -- coordinates of the point 'p'.
  diag :: Point -> Bool
  diag Point { getCoordinates = (x,y) } = x == y

  -- |'aboveDiag' 'p' return 'True' if 'x' < 'y', where 'x' and 'y' are the two
  -- coordinates of the point 'p'.
  aboveDiag :: Point -> Bool
  aboveDiag Point { getCoordinates = (x,y) } = x < y

  -- |'belowDiag' 'p' return 'True' if 'x' > 'y', where 'x' and 'y' are the two
  -- coordinates of the point 'p'.
  belowDiag :: Point -> Bool
  belowDiag Point { getCoordinates = (x,y) } = x > y

  -- |'getX' 'p' returns the x-coordinate of the point 'p'.
  getX :: Point -> X
  getX Point { getCoordinates = (x,_) } = x

  -- |'getY' 'p' returns the y-coordinate of the point 'p'.
  getY :: Point -> Y
  getY Point { getCoordinates = (_,y) } = y

  -- |'compX' 'p1' 'p2' compare the points 'p1' and 'p2' on their x-coordinates.
  compX :: Point -> Point -> Ordering
  compX p1 p2 = getX p1 `compare` getX p2

  -- |'p1' '@<-' 'p2'
  (@<-) :: Point -> Point -> Bool
  p1 @<- p2 = getX p1 < getX p2

  -- |'p1' '@>-' 'p2'
  (@>-) :: Point -> Point -> Bool
  p1 @>- p2 = getX p1 > getX p2

  -- |'compY' 'p1' 'p2' compare the points 'p1' and 'p2' on their y-coordinates.
  compY :: Point -> Point -> Ordering
  compY p1 p2 = getX p1 `compare` getX p2

  -- |'p1' '@<|' 'p2'
  (@<|) :: Point -> Point -> Bool
  p1 @<| p2 = getY p1 < getY p2

  -- |'p1' '@>|' 'p2'
  (@>|) :: Point -> Point -> Bool
  p1 @>| p2 = getY p1 > getY p2

  -- |Alias for '(@>|)'.
  aboveOf :: Point -> Point -> Bool
  p1 `above` p2 = p1 @>| p2

  -- |Alias for '(@<|)'.
  belowOf :: Point -> Point -> Bool
  p1 `below` p2 = p1 @<| p2

  -- |Alias for '(@<-)'.
  leftOf :: Point -> Point -> Bool
  p1 `left` p2 = p1 @<- p2

  -- |Alias for '(@>-)'.
  rightOf :: Point -> Point -> Bool
  p1 `right` p2 = p1 @>- p2

  -- |@p1 `dom` p2@ if the point 'p1' dominates the point 'p2'.
  --
  -- prop> p1 `dom` p2 == p1 @<- p2 && p1 @<| p2
  domBy :: Point -> Point -> Bool
  p1 `dom` p2 = p1 @<- p2 && p1 @<| p2
