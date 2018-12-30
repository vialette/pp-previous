module Data.Algorithm.PP.Combnatorics
(
  partitions
, evenPartitions
)
where

  -- |'partitions' 'k' 'n' returns all ordered partitions of @[n]@into 'k' parts.
  --
  -- >>> partitions 0 5
  -- []
  -- >>> partitions 1 5
  -- [[5]]
  -- >>> partitions 2 5
  -- [[1,4],[2,3],[3,2],[4,1]]
  -- >>> partitions 3 5
  -- [[1,1,3],[1,2,2],[1,3,1],[2,1,2],[2,2,1],[3,1,1]]
  -- >>> partitions 4 5
  -- [[1,1,1,2],[1,1,2,1],[1,2,1,1],[2,1,1,1]]
  -- >>> partitions 5 5
  -- [[1,1,1,1,1]]
  -- >>> partitions 6 5
  -- []
  partitions :: Int -> Int -> [[Int]]
  partitions k n
    | k > n || k <= 0 = []
    | otherwise       = aux k n
      where
        aux 1  n' = [[n']]
        aux k' n' = [x : xs | x <- [1..n'-k'+1], xs <- aux (k'-1) (n'-x)]

  -- |'evenPartitions' 'n' 'k' returns all ordered partitions of @[n]@ into 'k'
  -- even parts.
  --
  -- >>> evenPartitions 1 10
  -- [[10]]
  -- >>> evenPartitions 2 10
  -- [[2,8],[4,6],[6,4],[8,2]]
  -- >>> evenPartitions 3 10
  -- [[2,2,6],[2,4,4],[2,6,2],[4,2,4],[4,4,2],[6,2,2]]
  -- >>> evenPartitions 4 10
  -- [[2,2,2,4],[2,2,4,2],[2,4,2,2],[4,2,2,2]]
  -- >>> evenPartitions 5 10
  -- [[2,2,2,2,2]]
  -- >>> evenPartitions 6 10
  -- []
  evenPartitions :: Int -> Int -> [[Int]]
  evenPartitions k n
    | odd n || k <= 0 = []
    | otherwise       = aux k n
      where
        aux 1  n' = [[n']]
        aux k' n' = [x : xs | x <- [2,4..n'-(2*(k'-1))], xs <- aux (k'-1) (n'-x)]
