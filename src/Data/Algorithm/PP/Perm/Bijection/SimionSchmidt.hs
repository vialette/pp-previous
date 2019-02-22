module Data.Algorithm.PP.Perm.Bijection.SimionSchmidt
(
  simionSchmidt
, invSimionSchmidt
)
where

  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Tuple    as T

  import qualified Data.Algorithm.PP.Geometry.Point  as PP.Geometry.Point
  import qualified Data.Algorithm.PP.Perm            as PP.Perm
  import qualified Data.Algorithm.PP.Perm.Features   as PP.Perm.Features

  -- |'simionSchmidt' 'perm'
  --
  -- >>> simionSchmidt (mkPerm [6,7,3,4,1,2,5,8])
  -- [6,8,3,7,1,5,4,2]
  -- >>> let p = mkPerm [6,7,3,4,1,2,5,8] in invSimionSchmidt (simionSchmidt p) == p
  -- True
  simionSchmidt :: PP.Perm.Perm -> PP.Perm.Perm
  simionSchmidt perm = PP.Perm.fromPoints (minPoints ++ otherPoints')
    where
      minPoints    = PP.Perm.Features.leftToRightMinima perm
      otherPoints  = L.filter (`F.notElem` minPoints) $ PP.Perm.getPoints perm
      otherPoints' = uncurry PP.Geometry.Point.mk <$> L.zip xs ys
        where
          xs = fmap PP.Geometry.Point.getX otherPoints
          ys = PP.Geometry.Point.getY <$> PP.Geometry.Point.sortOnDescY otherPoints


  -- |'smallestsAbove' 'y' 'k' 'xs' returns at most 'k' elements from the list
  -- 'xs' that are strictly aboce 'y'.
  smallestsAbove :: PP.Geometry.Point.Point -> Int -> [PP.Geometry.Point.Point] -> [PP.Geometry.Point.Point]
  smallestsAbove p = aux []
    where
      y = PP.Geometry.Point.getY p

      aux acc 0 _  = L.reverse acc
      aux acc _ [] = L.reverse acc
      aux acc k (p' : ps')
        | y' > y    = aux (p' : acc) (k-1) ps'
        | otherwise = aux acc        k     ps'
          where
            y' = PP.Geometry.Point.getY p'

  -- invSimionSchmidt auxiliary function.
  -- The two lists 'ps' and 'ps'' must be sorted on x-cooredinates.
  step :: Int -> PP.Geometry.Point.Point -> [PP.Geometry.Point.Point] -> ([PP.Geometry.Point.Point], [PP.Geometry.Point.Point])
  step n p ps = (newPoints, selPoints)
    where
      x = PP.Geometry.Point.getX p
      selPoints = smallestsAbove p n ps
      newPoints = fmap (uncurry PP.Geometry.Point.mk) . L.zip [x+1..x+n] $ fmap PP.Geometry.Point.getY selPoints

  --
  intervals :: Int -> [PP.Geometry.Point.Point] -> [(PP.Geometry.Point.Point, Int)]
  intervals _ []             = []
  intervals n [p]            = [(p, n)]
  intervals n (p1 : p2 : ps) = (p1, n') : intervals (n-n') (p2 : ps)
    where
      n' = PP.Geometry.Point.getX p2 - PP.Geometry.Point.getX p1 - 1

  -- |'invSimionSchmidt' 'p'
  --
  -- >>> invSimionSchmidt (mkPerm [6,8,3,7,1,5,4,2])
  -- [6,7,3,4,1,2,5,8]
  -- >>> let p = mkPerm [6,8,3,7,1,5,4,2] in simionSchmidt (invSimionSchmidt p) == p
  --
  invSimionSchmidt :: PP.Perm.Perm -> PP.Perm.Perm
  invSimionSchmidt perm = PP.Perm.fromPoints (minPoints ++ otherPoints')
    where
      minPoints    = PP.Perm.Features.leftToRightMinima perm
      otherPoints  = PP.Geometry.Point.sortOnY . L.filter (`F.notElem` minPoints) $ PP.Perm.getPoints perm
      otherPoints' = F.concat . T.fst . F.foldl f ([], otherPoints) $ intervals (L.length otherPoints) minPoints
        where
          f (acc, ps) (p, n) = (ps' : acc, ps L.\\ ps'')
            where
              (ps', ps'') = step n p ps
