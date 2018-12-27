-- module Data.Algorithm.PP.Combnatorics
-- (
--   evenParts
-- )
-- where

  -- |'evenParts' 'n' 'k' returns all ordered partitions of @[n]@ into 'k'
  -- even parts.
  --
  -- >>> evenParts 10 1
  -- [[10]]
  -- >>> evenParts 10 2
  -- [[2,8],[4,6],[6,4],[8,2]]
  -- >>> evenParts 10 3
  -- [[2,2,6],[2,4,4],[2,6,2],[4,2,4],[4,4,2],[6,2,2]]
  -- >>> evenParts 10 4
  -- [[2,2,2,4],[2,2,4,2],[2,4,2,2],[4,2,2,2]]
  -- >>> evenParts 10 5
  -- [[2,2,2,2,2]]
  -- >>> evenParts 10 6
  -- []
  evenParts :: Int -> Int -> [[Int]]
  evenParts n k
    | odd n || k <= 0 = []
    | otherwise       = aux n k
      where
        aux n' 1  = [[n']]
        aux n' k' = [x : xs | x <- [2,4..n'-(2*(k'-1))], xs <- aux (n'-x) (k'-1)]
