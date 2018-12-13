module Main where

  import qualified Data.List          as L
  import qualified System.Environment as Environment
  import qualified System.IO          as IO

  import Data.Algorithm.PP.Perm
  import Data.Algorithm.PP.Perm.Complexity

  -- |'stringToInt' 's' converts the string 's' to integer
  stringToInt :: String -> Int
  stringToInt s = read s :: Int

  main :: IO ()
  main = do
    args <- Environment.getArgs
    let n = stringToInt( L.head args)
    IO.putStr . show $ maxComplexity (n `div` 2) n
