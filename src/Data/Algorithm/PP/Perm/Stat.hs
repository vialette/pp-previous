module Data.Algorithm.PP.Perm.Stat
(
  fixedPoints
, fixedPointsStat
, peaks
, peaksStat
, valleys
, valleysStat

, leftToRightMaxima
, leftToRightMaximaStat
, rightToLeftMaxima
, rightToLeftMaximaStat
)
where

  import qualified Data.List  as L
  import qualified Data.Tuple as T

  import qualified Data.Algorithm.PP.Perm as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List

  fixedPointsT :: [(Int, PP.Perm.T)] -> [(Int, PP.Perm.T)]
  fixedPointsT = L.filter (T.uncurry (==))

  fixedPoints :: PP.Perm.Perm -> [PP.Perm.T]
  fixedPoints = L.map T.snd . fixedPointsT . L.zip [1..] . PP.Perm.toList

  fixedPointsPos :: PP.Perm.Perm -> Int
  fixedPointsPos = L.map T.snd . fixedPointsT . L.zip [1..] . PP.Perm.toList

  fixedPointsStat :: PP.Perm.Perm -> Int
  fixedPointsStat = L.length . fixedPoints

  peaks :: PP.Perm.Perm -> [PP.Perm.T]
  peaks = L.map proj2 . L.filter f . PP.Utils.List.chunk3 . PP.Perm.toList
    where
      f [i, j, k]     = j > max i k
      proj2 [_, j, _] = j

  peaksStat :: PP.Perm.Perm -> Int
  peaksStat = L.length.peaks

  maxima :: PP.Perm.Perm -> [PP.Perm.T]
  maxima = peaks

  maximaStat :: PP.Perm.Perm -> Int
  maximaStat = peaksStat

  valleys :: PP.Perm.Perm -> [PP.Perm.T]
  valleys = L.map proj2 . L.filter f . PP.Utils.List.chunk3 . PP.Perm.toList
    where
      f [i, j, k]     = j < max i k
      proj2 [_, j, _] = j

  valleysStat :: PP.Perm.Perm -> Int
  valleysStat = L.length.valleys

  minima :: PP.Perm.Perm -> [PP.Perm.T]
  minima = valleys

  minimaStat :: PP.Perm.Perm -> Int
  minimaStat = valleysStat

  leftToRightMaxima :: PP.Perm.Perm -> [PP.Perm.T]
  leftToRightMaxima = L.reverse . go 0 [] . PP.Perm.toList
    where
      go m acc [] = acc
      go m acc (x : xs)
        | x > m     = go x (x : acc) xs
        | otherwise = go m acc       xs

  leftToRightMaximaStat :: PP.Perm.Perm -> Int
  leftToRightMaximaStat = L.length . leftToRightMaxima

  rightToLeftMaxima :: PP.Perm.Perm -> [PP.Perm.T]
  rightToLeftMaxima = L.reverse . leftToRightMaxima . PP.Perm.rev

  rightToLeftMaximaStat :: PP.Perm.Perm -> Int
  rightToLeftMaximaStat = L.length . rightToLeftMaxima
