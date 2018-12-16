module Main where

  import qualified Data.Foldable      as F
  import qualified Data.List          as L
  import qualified System.Environment as Environment
  import qualified System.IO          as IO

  import Data.Algorithm.PP.Perm
  import Data.Algorithm.PP.Perm.Complexity

  stringToInt :: String -> Int
  stringToInt s = read s :: Int

  main :: IO ()
  main = do
    args <- Environment.getArgs
    let from = stringToInt $ L.head args
    let to   = stringToInt . L.head $ L.tail args
    mapM_ IO.putStr [show (n, maxComplexity (n `div` 2) n) ++ "\n\n" | n <- [from,from+2..to]]
