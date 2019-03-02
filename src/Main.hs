module Main where

  import qualified Data.Foldable      as F
  import qualified Data.List          as L
  import qualified System.Environment as Environment
  import qualified System.IO          as IO

  import Data.Algorithm.PP.Perm
  import Data.Algorithm.PP.Perm.Complexity
  import Data.Algorithm.PP.Perm.ShuffleSquare

  stringToInt :: String -> Int
  stringToInt s = read s :: Int

  f :: Int -> [Perm] -> (Int, [Perm])
  f k = F.foldr aux (0, [])
    where
      aux p (maxSoFar, acc)
        | m > maxSoFar  = (m, [p])
        | m == maxSoFar = (maxSoFar, p : acc)
        | otherwise     = (maxSoFar, acc)
          where
            m = complexityStat k p

  main :: IO ()
  main = do
    args <- Environment.getArgs
    let n = stringToInt( L.head args)
    let res = [(n, f (n `div` 2) (shuffleSquares n)) | n <- [2,4..n]]
    mapM_ (IO.putStr . show) res