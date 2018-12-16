module Main where

  import qualified Data.Foldable      as F
  import qualified Data.List          as L
  import qualified System.Environment as Environment
  import qualified System.IO          as IO

  import Data.Algorithm.PP.Perm
  import Data.Algorithm.PP.Perm.ShuffleSquare

  stringToInt :: String -> Int
  stringToInt s = read s :: Int

  maxSquareRootComplexity :: Int -> (Int, [Perm])
  maxSquareRootComplexity = F.foldr aux (0, []) . shuffleSquares
    where
      aux p (maxSoFar, acc)
        | m > maxSoFar  = (m, [p])
        | m == maxSoFar = (maxSoFar, p : acc)
        | otherwise     = (maxSoFar, acc)
          where
            m = shuffleSquareRootsStat p

  main :: IO ()
  main = do
    args <- Environment.getArgs
    let from = stringToInt $ L.head args
    let to   = stringToInt . L.head $ L.tail args
    mapM_ IO.putStr [show (n, maxSquareRootComplexity n) ++ "\n\n" | n <- [from,from+2..to]]
