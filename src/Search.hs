module Main where

import qualified Data.Foldable      as F
import qualified Data.List          as L
import qualified System.Environment as Environment
import qualified System.IO          as IO

import Data.Algorithm.PP.Perm                      as PP.Perm
import Data.Algorithm.PP.Perm.Search               as PP.Perm.Search
import Data.Algorithm.PP.Perm.Class.SuperSeparable as PP.Perm.Class.SuperSeparable
import Data.Algorithm.PP.Perm.Generator            as PP.Perm.Generator
import Data.Algorithm.PP.SuperSeparatingTree       as PP.SuperSeparatingTree


check :: PP.Perm.Perm -> Bool
check p = F.or [PP.Perm.Search.search q p | q <- qs]
  where
    qs = [ PP.Perm.mk [2,4,1,5,3]
         , PP.Perm.mk [2,5,3,1,4]
         , PP.Perm.mk [3,1,5,2,4]
         , PP.Perm.mk [3,5,1,4,2]
         , PP.Perm.mk [4,1,3,5,2]
         , PP.Perm.mk [4,2,5,1,3]]

stringToInt :: String -> Int
stringToInt s = read s :: Int

main :: IO ()
main = do
  args <- Environment.getArgs
  let n = stringToInt $ L.head args
  let ps = L.filter check . L.filter (PP.Perm.Class.SuperSeparable.superSeparable) $ PP.Perm.Generator.perms n
  IO.putStrLn "superseparable permutations that do not avoid 24143, 25314, 31524,35142, 41352 and 42513:"
  IO.putStrLn $ show ps
  let ps' = L.filter (not . check) . L.filter (not . PP.Perm.Class.SuperSeparable.superSeparable) $ PP.Perm.Generator.perms n
  IO.putStrLn "non-superseparable permutations that do contain 24143, 25314, 31524,35142, 41352 or 42513:"
  IO.putStrLn $ show ps'