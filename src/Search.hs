module Main where

import Control.Arrow
import qualified Data.Foldable      as F
import qualified Data.List          as L
import qualified System.Environment as Environment
import qualified System.IO          as IO

import Data.Algorithm.PP.Perm           as PP.Perm
import Data.Algorithm.PP.Perm.Search    as PP.Perm.Search
import Data.Algorithm.PP.Perm.Generator as PP.Perm.Generator
import Data.Algorithm.PP.PTree     as PP.PTree


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

--count :: Int -> [(Int, Int)]
count = L.map shape . L.group . L.sort . L.map maxNode . PP.Perm.Generator.perms
  where
    maxNode = PP.Perm.len . F.maximum . PP.PTree.labels . PP.PTree.mk
    shape   = L.head &&& L.length

--map (sum . map snd) . groupBy ((==) `on` fst) . sort

main :: IO ()
main = do
  args <- Environment.getArgs
  let n = stringToInt $ L.head args
  mapM_ print $ count n