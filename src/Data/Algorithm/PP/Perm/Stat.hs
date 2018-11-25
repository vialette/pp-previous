module Data.Algorithm.PP.Perm.Stat
(
  fixedPoints
, fixedPointsPos
, fixedPointsStat

, peaks
, peaksPos
, peaksStat
, maxima
, maximaPos
, maximaStat

, valleys
, valleysPos
, valleysStat
, minima
, minimaPos
, minimaStat

, leftToRightMaxima
, leftToRightMaximaPos
, leftToRightMaximaStat

, rightToLeftMaxima
, rightToLeftMaximaPos
, rightToLeftMaximaStat
)
where

  import qualified Data.List  as L
  import qualified Data.Tuple as T

  import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point
  import qualified Data.Algorithm.PP.Perm           as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List     as PP.Utils.List

  --
  fixedPoints' :: PP.Perm.Perm -> [PP.Geometry.Point.Point]
  fixedPoints' = L.filter PP.Geometry.Point.diagonal . PP.Perm.toPoints

  -- |
  fixedPoints :: PP.Perm.Perm -> [PP.Perm.T]
  fixedPoints = L.map T.snd . fixedPoints'

  -- |
  fixedPointsPos :: PP.Perm.Perm -> [Int]
  fixedPointsPos = L.map T.fst . fixedPoints'

  -- |
  fixedPointsStat :: PP.Perm.Perm -> Int
  fixedPointsStat = L.length . fixedPoints

  --
  peaks' :: PP.Perm.Perm -> [(Int, PP.Perm.T)]
  peaks' = L.map proj2 . L.filter f . PP.Utils.List.chunk3 . L.zip [1..] . PP.Perm.toList
    where
      f [p1, p2, p3]  = T.snd p1 < T.snd p2 && T.snd p2 > T.snd p3
      proj2 [_, j, _] = j

  -- |
  peaks :: PP.Perm.Perm -> [PP.Perm.T]
  peaks = L.map T.snd . peaks'

  -- |
  peaksPos :: PP.Perm.Perm -> [Int]
  peaksPos = L.map T.fst . peaks'

  -- |
  peaksStat :: PP.Perm.Perm -> Int
  peaksStat = L.length . peaks

  -- |
  maxima :: PP.Perm.Perm -> [PP.Perm.T]
  maxima = peaks

  -- | Alias for 'peaksPos'.
  maximaPos :: PP.Perm.Perm -> [Int]
  maximaPos = peaksPos

  -- | Alias for 'peaksStat'.
  maximaStat :: PP.Perm.Perm -> Int
  maximaStat = peaksStat

  --
  valleys' :: PP.Perm.Perm -> [(Int, PP.Perm.T)]
  valleys' = L.map proj2 . L.filter f . PP.Utils.List.chunk3 . L.zip [1..] . PP.Perm.toList
    where
      f [p1, p2, p3]  = T.snd p1 > T.snd p2 && T.snd p2 < T.snd p3
      proj2 [_, j, _] = j

  -- |
  valleys :: PP.Perm.Perm -> [PP.Perm.T]
  valleys = L.map T.snd . valleys'

  -- |
  valleysPos :: PP.Perm.Perm -> [Int]
  valleysPos = L.map T.snd . valleys'

  -- |
  valleysStat :: PP.Perm.Perm -> Int
  valleysStat = L.length.valleys

  -- | Alias for 'valleys'.
  minima :: PP.Perm.Perm -> [PP.Perm.T]
  minima = valleys

  -- | Alias for 'valleysPos'.
  minimaPos :: PP.Perm.Perm -> Int
  minimaPos = valleysStat

  -- | Alias for 'valleysStat'.
  minimaStat :: PP.Perm.Perm -> Int
  minimaStat = valleysStat

  leftToRightMaxima' :: PP.Perm.Perm -> [(Int, PP.Perm.T)]
  leftToRightMaxima' = L.reverse . go 0 [] . L.zip [1..] . PP.Perm.toList
    where
      go m acc [] = acc
      go m acc ((i, x) : xs)
        | x > m     = go x ((i, x) : acc) xs
        | otherwise = go m acc            xs

  -- |
  leftToRightMaxima :: PP.Perm.Perm -> [PP.Perm.T]
  leftToRightMaxima = L.map T.snd . leftToRightMaxima'

  -- |
  leftToRightMaximaPos :: PP.Perm.Perm -> [Int]
  leftToRightMaximaPos = L.map T.fst . leftToRightMaxima'

  -- |
  leftToRightMaximaStat :: PP.Perm.Perm -> Int
  leftToRightMaximaStat = L.length . leftToRightMaxima

  -- |
  rightToLeftMaxima :: PP.Perm.Perm -> [PP.Perm.T]
  rightToLeftMaxima = L.reverse . leftToRightMaxima . PP.Perm.rev

  -- |
  rightToLeftMaximaPos :: PP.Perm.Perm -> [Int]
  rightToLeftMaximaPos = L.reverse . leftToRightMaximaPos . PP.Perm.rev

  -- |
  rightToLeftMaximaStat :: PP.Perm.Perm -> Int
  rightToLeftMaximaStat = L.length . rightToLeftMaxima
