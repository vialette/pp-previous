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
, xCoord
, yCoord
)
where

  import qualified Data.List as L

  -- |x-coordinate type.
  type X = Int

  -- |y-coordinate type.
  type Y = Int

  -- |2D point type.
  newtype Point = Point { getCoords :: (X, Y) } deriving (Eq, Ord)

  instance Show Point  where
    show Point { getCoords = (x,y) } = "(" ++ show x ++ "," ++ show y ++ ")"

  -- |'mk' 'x' 'y' makes a point with given 'x' and 'y' coordinates.
  mk :: X -> Y -> Point
  mk x y = Point { getCoords = (x, y) }

  -- |'mks' 'xs' 'ys'
  mks :: [X] -> [Y] -> [Point]
  mks = L.zipWith mk

  -- |'mksX' 'xs'
  mksX :: [X] -> [Point]
  mksX xs = L.zipWith mk xs [1..]

  -- |'mksY' 'ys'
  mksY :: [Y] -> [Point]
  mksY = L.zipWith mk [1..]

  -- |'symm' 'p'
  symm :: Point -> Point
  symm Point { getCoords = (x, y) } = Point { getCoords = (y, x) }

  -- |'diag' 'p' return 'True' if 'x' == 'y', where 'x' and 'y' are the two
  -- coordinates of the point 'p'.
  diag :: Point -> Bool
  diag Point { getCoords = (x,y) } = x == y

  -- |'aboveDiag' 'p' return 'True' if 'x' < 'y', where 'x' and 'y' are the two
  -- coordinates of the point 'p'.
  aboveDiag :: Point -> Bool
  aboveDiag Point { getCoords = (x,y) } = x < y

  -- |'belowDiag' 'p' return 'True' if 'x' > 'y', where 'x' and 'y' are the two
  -- coordinates of the point 'p'.
  belowDiag :: Point -> Bool
  belowDiag Point { getCoords = (x,y) } = x > y

  -- |'xCoord' 'p' returns the x-coordinate of the point 'p'.
  xCoord :: Point -> X
  xCoord Point { getCoords = (x,_) } = x

  -- |'yCoord' 'p' returns the y-coordinate of the point 'p'.
  yCoord :: Point -> Y
  yCoord Point { getCoords = (_,y) } = y

  -- |'compX' 'p1' 'p2' compare the points 'p1' and 'p2' on their x-coordinates.
  compX :: Point -> Point -> Ordering
  compX p1 p2 = xCoord p1 `compare` xCoord p2

  -- |'p1' '@<-' 'p2'
  (@<-) :: Point -> Point -> Bool
  p1 @<- p2 = xCoord p1 < xCoord p2

  -- |'p1' '@>-' 'p2'
  (@>-) :: Point -> Point -> Bool
  p1 @>- p2 = xCoord p1 > xCoord p2

  -- |'compY' 'p1' 'p2' compare the points 'p1' and 'p2' on their y-coordinates.
  compY :: Point -> Point -> Ordering
  compY p1 p2 = xCoord p1 `compare` xCoord p2

  -- |'p1' '@<|' 'p2'
  (@<|) :: Point -> Point -> Bool
  p1 @<| p2 = yCoord p1 < yCoord p2

  -- |'p1' '@>|' 'p2'
  (@>|) :: Point -> Point -> Bool
  p1 @>| p2 = yCoord p1 > yCoord p2

  -- |Alias for '(@>|)'.
  aboveOf :: Point -> Point -> Bool
  p1 `aboveOf` p2 = p1 @>| p2

  -- |Alias for '(@<|)'.
  belowOf :: Point -> Point -> Bool
  p1 `belowOf` p2 = p1 @<| p2

  -- |Alias for '(@<-)'.
  leftOf :: Point -> Point -> Bool
  p1 `leftOf` p2 = p1 @<- p2

  -- |Alias for '(@>-)'.
  rightOf :: Point -> Point -> Bool
  p1 `rightOf` p2 = p1 @>- p2

  -- |@p1 `dom` p2@ if the point 'p1' dominates the point 'p2'.
  --
  -- prop> p1 `dom` p2 == p1 @<- p2 && p1 @<| p2
  domBy :: Point -> Point -> Bool
  p1 `domBy` p2 = p1 @<- p2 && p1 @<| p2
