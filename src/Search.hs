module Main where

  import qualified Data.Foldable      as F
  import qualified Data.List          as L
  import qualified System.Environment as Environment
  import qualified System.IO          as IO

  import Data.Algorithm.PP.Geometry.Point    as PP.Geometry.Point
  import Data.Algorithm.PP.Perm              as PP.Perm
  import Data.Algorithm.PP.Perm.Features     as PP.Perm.Features
  import Data.Algorithm.PP.Perm.Organization as PP.Perm.Organization
  import Data.Algorithm.PP.Perm.Generator    as PP.Perm.Generator


  data Color = Red | Blue deriving (Show, Eq, Ord)

  type Arc = (PP.Geometry.Point.Point, PP.Geometry.Point.Point)

  data Arcs = Arcs { getCountInArcs :: Int
                   , getCountOutArcs :: Int
                   , getOutArcs :: [Arc]
                   } deriving (Show)

  data Node = Node { point               :: PP.Geometry.Point.Point
                   , getCountInBlueArcs  :: Int
                   , getCountOutBlueArcs :: Int
                   , getOutBlueArcs      :: [Node]
                   , getCountInRedArcs   :: Int
                   , getCountOutRedArcs  :: Int
                   , getOutRedArcs       :: [Node]
                   } deriving (Show, Eq)

  mkArc :: (Int, Int) -> (Int, Int) -> Arc
  mkArc (i, j) (k, l) = (PP.Geometry.Point.mk i j, PP.Geometry.Point.mk k l)

  buildGraph :: PP.Perm.Perm -> [(PP.Geometry.Point.Point, PP.Geometry.Point.Point)]
  buildGraph p = blueArcs ++ redArcs
    where
      n  = PP.Perm.len p
      xo = L.zip [1..] $ PP.Perm.Organization.xOrganization p
      yo = L.zip [1..] $ PP.Perm.Organization.yOrganization p

      blueArcs = [mkArc (i, j) (i+d, j+1) | (j, d) <- yo, i <- [1..n-d]] ++
                 [mkArc (i, j) (i-d, j+1) | (j, d) <- yo, i <- [d..n]]

      redArcs  = [mkArc (i, j) (i+1, j+d) | (i, d) <- xo, j <- [1..n-d]] ++
                 [mkArc (i, j) (i+1, j-d) | (i, d) <- xo, j <- [d..n]]

  main :: IO ()
  main = do
    let p = PP.Perm.mk [2, 5, 1, 4, 6, 3]
    let g = buildGraph p
    IO.putStrLn $ show g
