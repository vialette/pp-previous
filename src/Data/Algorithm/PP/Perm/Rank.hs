module Data.Algorithm.PP.Perm.Rank
(
  rank1
, unrank1

, rank2
, unrank2

, rand
)
where

  import System.Random
  import Data.Array ((!), (//))
  import qualified Data.Array as Array
  import qualified Data.List  as L

  import qualified Data.Algorithm.PP.Perm       as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List as PP.Utils.List

  mkArray :: [a] -> Array.Array Int a
  mkArray xs = Array.array (0, n-1) $ L.zip [0..] xs
    where
      n = L.length xs

  swap :: (Array.Ix i) => i -> i -> Array.Array i x -> Array.Array i x
  swap i j a = a // [(i, xj), (j, xi)]
    where
      xi = a ! i
      xj = a ! j

  rank' :: Int -> Array.Array Int Int -> Array.Array Int Int -> (Int, Array.Array Int Int, Array.Array Int Int)
  rank' n a b = (s, a', b')
    where
      s  = a ! (n-1)
      a' = swap (n-1) (b ! (n-1)) a
      b' = swap s     (n-1)       b

  -- |'rank1' 'p' returns the rank of the permutation 'p' according to Algorithm
  -- __rank1__ from
  -- Wendy J. Myrvold, Frank Ruskey. Ranking and unranking permutations in linear time. Inf. Process. Lett. 79(6): 281-284 (2001)
  --
  -- >>> let n = 3 in [(p, rank1 p) | p <- perms n]
  -- [([1,2,3],5),([2,1,3],2),([3,2,1],3),([2,3,1],0),([3,1,2],1),([1,3,2],4)]
  -- >>> let n = 3 in and [p == unrank1 n (rank1 p) | p <- perms n]
  -- True
  rank1 :: PP.Perm.Perm -> Int
  rank1 p = rank1' (PP.Perm.len p) a b
    where
      a = mkArray . L.map (\ x -> x-1) $ PP.Perm.getList p
      b = mkArray . L.map (\ y -> y-1) . PP.Perm.getList $ PP.Perm.inv p

  rank1' :: Int -> Array.Array Int Int -> Array.Array Int Int -> Int
  rank1' 1 _  _  = 0
  rank1' n a b = s + (n * rank1' (n-1) a' b')
    where
      (s, a', b') = rank' n a b

  -- |'unrank1' 'k' 'n' returns the permutation of length 'n' with rank 'k' according
  -- to Algorithm __unrank1__ from
  -- Wendy J. Myrvold, Frank Ruskey. Ranking and unranking permutations in linear time. Inf. Process. Lett. 79(6): 281-284 (2001)
  --
  -- >>> let n = 3 in [(i, unrank1 n i) | i <- [0..product [1..n]-1]]
  -- [(0,Just [2,3,1]),(1,Just [3,1,2]),(2,Just [2,1,3]),(3,Just [3,2,1]),(4,Just [1,3,2]),(5,Just [1,2,3])]
  -- >>> let n = 3 in and [i == (rank1 . fromJust . unrank1 n) i | i <- [0..product [1..n]-1]]
  -- True
  unrank1 :: Int -> Int -> Maybe PP.Perm.Perm
  unrank1 n r
    | r < 0 || r >= product [1..n] = Nothing
    | otherwise                    = Just . PP.Perm.mkPerm . unrank1' n r $ mkArray [0..n-1]

  unrank1' :: Int -> Int -> Array.Array Int Int -> [Int]
  unrank1' n r a
    | n > 0     = unrank1' (n-1) (r `div` n) (swap (n-1) (r `mod` n) a)
    | otherwise = Array.elems a

  -- |'rank2' 'p' returns the rank of the permutation 'p' according to Algorithm
  -- __rank2__ from
  -- Wendy J. Myrvold, Frank Ruskey. Ranking and unranking permutations in linear time. Inf. Process. Lett. 79(6): 281-284 (2001)
  --
  -- >>> let n = 3 in [(p, rank2 p) | p <- perms n]
  -- [([1,2,3],5),([2,1,3],4),([3,2,1],1),([2,3,1],0),([3,1,2],2),([1,3,2],3)]
  -- >>> let n = 3 in and [p == unrank2 n (rank2 p) | p <- perms n]
  -- True
  rank2 :: PP.Perm.Perm -> Int
  rank2 p = rank2' (PP.Perm.len p) a b
    where
      a = mkArray . L.map (\ x -> x-1) $ PP.Perm.getList p
      b = mkArray . L.map (\ y -> y-1) . PP.Perm.getList $ PP.Perm.inv p

  rank2' :: Int -> Array.Array Int Int -> Array.Array Int Int -> Int
  rank2' 1 _  _  = 0
  rank2' n a b = s * product [1..n-1] + rank2' (n-1) a' b'
    where
      (s, a', b') = rank' n a b

  -- |'unrank2' 'k' 'n' returns the permutation of length 'n' with rank 'k' according
  -- to Algorithm __unrank2__ from
  -- Wendy J. Myrvold, Frank Ruskey. Ranking and unranking permutations in linear time. Inf. Process. Lett. 79(6): 281-284 (2001)
  --
  -- >>> let n = 3 in [(i, unrank2 3 i) | i <- [0..product [1..n]-1]]
  -- [(0,[2,3,1]),(1,[3,2,1]),(2,[3,1,2]),(3,[1,3,2]),(4,[2,1,3]),(5,[1,2,3])]
  -- >>>  let n = 3 in and [i == (rank2 . fromJust . unrank2 n) i | i <- [0..product [1..n]-1]]
  -- True
  unrank2 :: Int -> Int -> Maybe PP.Perm.Perm
  unrank2 n r
    | r < 0 || r >= product [1..n] = Nothing
    | otherwise                    = Just . PP.Perm.mkPerm . unrank2' n r $ mkArray [0..n-1]

  unrank2' :: Int -> Int -> Array.Array Int Int -> [Int]
  unrank2' n r a
    | n > 0     = unrank2' (n-1) (r - s*product [1..n-1]) (swap (n-1) s a)
    | otherwise = Array.elems a
      where
        s = r `div` product [1..n-1]

  randRank :: Array.Array Int Int -> (PP.Perm.Perm -> Int) -> PP.Perm.Perm -> Int
  randRank a f = (!) a . f

  randUnrank :: Array.Array Int Int -> (Int -> Int -> Maybe PP.Perm.Perm) -> Int -> Int -> Maybe PP.Perm.Perm
  randUnrank a f n = f n . (!) a

  rand :: RandomGen g => g -> Int -> ((PP.Perm.Perm -> Int, Int -> Int -> Maybe PP.Perm.Perm), g)
  rand g n = ((randRank a rank2, randUnrank a' unrank2), g')
    where
      b        = product [1..n]
      (xs, g') = PP.Utils.List.randomShuffle g [0..b-1]
      a        = Array.array (0, b-1) $ L.zip [0..] xs
      a'       = Array.array (0, b-1) $ L.zip xs    [0..]
