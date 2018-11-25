module Data.Algorithm.PP.Perm.Stat
(
  fixedPoints
, fixedPointsStat

, ascents
, ascentsStat

, descents
, descentsStat

, excedances
, excedancesStat
, weakExcedances
, weakExcedancesStat

, peaks
, peaksStat
, maxima
, maximaStat

, valleys
, valleysStat
, minima
, minimaStat

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

  --
  fixedPointsAux :: PP.Perm.Perm -> [(Int, PP.Perm.T)]
  fixedPointsAux = L.filter (T.uncurry (==)) . L.zip [1..] . PP.Perm.toList

  -- |'fixedPoints' 'p'
  --
  -- >>> fixedPoints (mk [3,2,1,6,5,4])
  -- [2,5]
  -- >>> fixedPoints (mk [3,1,5,6,2,4])
  -- []
  fixedPoints :: PP.Perm.Perm -> [PP.Perm.T]
  fixedPoints = L.map T.snd . fixedPointsAux


  -- |'fixedPointsStat' 'p'
  --
  -- >>> fixedPointsStat (mk [3,2,1,6,5,4])
  -- 2
  -- >>> fixedPointsStat (mk [3,1,5,6,2,4])
  -- 0
  fixedPointsStat :: PP.Perm.Perm -> Int
  fixedPointsStat = L.length . fixedPoints

  -- auxilliary function for 'ascents' and 'descents' functions.
  ascentsDescentsAux :: (PP.Perm.T -> PP.Perm.T -> Bool) -> PP.Perm.Perm -> [(Int, PP.Perm.T)]
  ascentsDescentsAux cmp = L.map T.fst . L.filter f . PP.Utils.List.chunk2 . L.zip [1..] . PP.Perm.toList
    where
      f ((_, x), (_, y)) = x `cmp` y

  -- |'ascents' 'p' returns the ascents of the permutation 'p'
  -- (i.e. the positions of 'p' where the following value is bigger than the current
  -- one).
  --
  -- >>> ascents (mk [3,1,5,6,2,4])
  -- [(2,1),(3,5),(5,2)]
  ascents :: PP.Perm.Perm -> [(Int, PP.Perm.T)]
  ascents = ascentsDescentsAux (<)

  -- |'ascentsStat' 'p'.
  --
  -- >>> ascentsStat (mk [3,1,5,6,2,4])
  -- 3
  ascentsStat :: PP.Perm.Perm -> Int
  ascentsStat = L.length . ascents

  -- |'descents' 'p' returns the ascents of the permutation 'p'
  -- (i.e. the positions of 'p' where the following value is smaller than the current
  -- one).
  --
  -- >>> descents (mk [3,1,5,6,2,4])
  -- [(1,3),(4,6)]
  descents :: PP.Perm.Perm -> [(Int, PP.Perm.T)]
  descents = ascentsDescentsAux (>)

  -- |'descentsStat' 'p'.
  --
  -- >>> descentsStat (mk [3,1,5,6,2,4])
  -- 2
  descentsStat :: PP.Perm.Perm -> Int
  descentsStat = L.length . descents

  -- Auxilliary function for 'excedances' and 'weakExcedances' fucntions.
  excedancesAux :: (PP.Perm.T -> PP.Perm.T -> Bool) -> PP.Perm.Perm -> [(Int, PP.Perm.T)]
  excedancesAux = L.map T.fst . L.filter f . L.zip [1..] . PP.Perm.toList
    where
      f (i, x) = x `cmp` i

  -- |'excedances' 'p'
  --
  -- >>>
  excedances :: PP.Perm.Perm -> [(Int, PP.Perm.T)]
  excedances = excedancesAux (>)

  excedancesStat :: PP.Perm.Perm -> Int
  excedancesStat = L.length . excedances

  weakExcedances :: PP.Perm.Perm -> [(Int, PP.Perm.T)]
  weakExcedances = excedancesAux (>=)

  weakExcedancesStat :: PP.Perm.Perm -> Int
  weakExcedancesStat = L.length . weakExcedances

  -- , excedances
  -- , excedancesStat
  -- , weakExcedances
  -- , weakExcedancesStat

  -- |'peaks' 'p'
  --
  -- >>> peaks (mk [4,6,1,3,2,5])
  -- [(2,6),(4,3)]
  peaks :: PP.Perm.Perm -> [(Int, PP.Perm.T)]
  peaks = L.map proj2 . L.filter f . PP.Utils.List.chunk3 . L.zip [1..] . PP.Perm.toList
    where
      f (p1, p2, p3)  = T.snd p1 < T.snd p2 && T.snd p2 > T.snd p3
      proj2 (_, j, _) = j

  -- |'peaksStat' 'p'
  --
  -- >>> peaksStat (mk [4,6,1,3,2,5])
  -- 2
  peaksStat :: PP.Perm.Perm -> Int
  peaksStat = L.length . peaks

  -- |Alias for 'peaks'.
  maxima :: PP.Perm.Perm -> [(Int, PP.Perm.T)]
  maxima = peaks

  -- | Alias for 'peaksStat'.
  maximaStat :: PP.Perm.Perm -> Int
  maximaStat = peaksStat

  -- |'valleys' 'p'
  --
  -- >>> valleys (mk [3,1,5,2,6,4])
  -- [(2,1),(4,2)]
  valleys :: PP.Perm.Perm -> [(Int, PP.Perm.T)]
  valleys = L.map proj2 . L.filter f . PP.Utils.List.chunk3 . L.zip [1..] . PP.Perm.toList
    where
      f (p1, p2, p3)  = T.snd p1 > T.snd p2 && T.snd p2 < T.snd p3
      proj2 (_, j, _) = j

  -- |'valleysStat' 'p'
  --
  -- >>> valleysStat(mk [3,1,5,2,6,4])
  -- 2
  valleysStat :: PP.Perm.Perm -> Int
  valleysStat = L.length . valleys

  -- | Alias for 'valleys'.
  minima :: PP.Perm.Perm -> [(Int, PP.Perm.T)]
  minima = valleys

  -- | Alias for 'valleysStat'.
  minimaStat :: PP.Perm.Perm -> Int
  minimaStat = valleysStat

  -- |'leftToRightMinima' 'p'
  --
  -- >>> leftToRightMaxima (mk [3,1,5,2,6,4])
  -- [(1,3),(3,5),(5,6)]
  leftToRightMaxima :: PP.Perm.Perm -> [(Int, PP.Perm.T)]
  leftToRightMaxima = L.reverse . go 0 [] . L.zip [1..] . PP.Perm.toList
    where
      go m acc [] = acc
      go m acc ((i, x) : xs)
        | x > m     = go x ((i, x) : acc) xs
        | otherwise = go m acc            xs

  -- |'leftToRightMinima' 'p'
  --
  -- >>> leftToRightMaximaStat (mk [3,1,5,2,6,4])
  -- 3
  leftToRightMaximaStat :: PP.Perm.Perm -> Int
  leftToRightMaximaStat = L.length . leftToRightMaxima

  -- |'rightToLeftMaxima' 'p'
  --
  -- >>> rightToLeftMaxima (mk [3,1,5,2,6,4])
  -- [(2,6),(1,4)]
  rightToLeftMaxima :: PP.Perm.Perm -> [(Int, PP.Perm.T)]
  rightToLeftMaxima = L.reverse . leftToRightMaxima . PP.Perm.rev

  -- |'rightToLeftMaximaStat' 'p'
  --
  -- >>> rightToLeftMaximaStat (mk [3,1,5,2,6,4])
  -- 2
  rightToLeftMaximaStat :: PP.Perm.Perm -> Int
  rightToLeftMaximaStat = L.length . rightToLeftMaxima
