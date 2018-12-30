import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Tuple as T

data Step a = UpStep a | DownStep a deriving (Show, Eq, Ord)

newtype Path a = Path { getSteps :: [Step a] } deriving (Show, Eq, Ord)

instance Semigroup (Path a) where
  p <> p' = mk (getSteps p ++ getSteps p')

instance Monoid (Path a) where
    mempty = mk []

evenPartitions :: Int -> Int -> [[Int]]
evenPartitions k n
  | odd n || k <= 0 = []
  | otherwise       = aux k n
    where
      aux 1  n' = [[n']]
      aux k' n' = [x : xs | x <- [2,4..n'-(2*(k'-1))], xs <- aux (k'-1) (n'-x)]

partitions :: Int -> Int -> [[Int]]
partitions k n
  | k > n || k <= 0 = []
  | otherwise       = aux k n
    where
      aux 1  n' = [[n']]
      aux k' n' = [x : xs | x <- [1..n'-k'+1], xs <- aux (k'-1) (n'-x)]

mk :: [Step a] -> Path a
mk ss = Path { getSteps = ss }

flipStep :: Step a -> Step a
flipStep (UpStep x)   = DownStep x
flipStep (DownStep x) = UpStep x

rev :: Path a -> Path a
rev = mk . L.reverse . fmap flipStep . getSteps

mirror :: Path a -> Path a
mirror = mk . fmap flipStep . getSteps

map :: (Step a -> Step b) -> Path a -> Path b
map f = mk . fmap f . getSteps

paths :: Int -> [Path ()]
paths 0 = []
paths n = mk <$> aux n
  where
    aux :: Int -> [[Step ()]]
    aux 0 = [[]]
    aux n' = (:) <$> [UpStep (), DownStep ()] <*> aux (n'-1)

predicatePosPaths :: (Int -> Bool) -> Int -> [Path ()]
predicatePosPaths p n
  | n <= 0    = []
  | otherwise = mk <$> aux n 0
    where
      aux :: Int -> Int -> [[Step ()]]
      aux 0  _ = [[]]
      aux n' h
        | p h       = upSteps
        | otherwise = upSteps `mappend` downSteps
        where
          upSteps :: [[Step ()]]
          upSteps   = (:) <$> [UpStep ()]   <*> aux (n'-1) (h+1)

          downSteps :: [[Step ()]]
          downSteps = (:) <$> [DownStep ()] <*> aux (n'-1) (h-1)

strictlyPosPaths :: Int -> [Path ()]
strictlyPosPaths = predicatePosPaths (<= 1)

posPaths :: Int -> [Path ()]
posPaths = predicatePosPaths (== 1)

returnPosPaths :: Int -> Int -> [Path ()]
returnPosPaths k n = F.concatMap f $ evenPartitions (k+1) n
    where
      f :: [Int] -> [Path ()]
      f xs = (mk . mconcat) <$> sequence [[[UpStep ()] `mappend` ss `mappend` [DownStep ()] | ss <- aux (x-2)] | x <- xs]

      aux :: Int-> [[Step ()]]
      aux 0 = [[]]
      aux n' = [[UpStep ()] `mappend` ss `mappend` [DownStep ()] `mappend` ss' | m <- [0..n'-2]
                                                                               , ss  <- aux m
                                                                               , ss' <- aux (n'-2-m)]

anyReturnPosPaths :: Int -> [Path ()]
anyReturnPosPaths n = F.concat [returnPosPaths k n | k <- [0..n `div`2]]

strictlyNegPaths :: Int -> [Path ()]
strictlyNegPaths = fmap mirror . strictlyNegPaths

negPaths :: Int -> [Path ()]
negPaths = fmap mirror . posPaths

returnNegPaths :: Int -> Int -> [Path ()]
returnNegPaths k = fmap mirror . returnPosPaths k

anyReturnNegPaths :: Int -> [Path ()]
anyReturnNegPaths = fmap mirror . anyReturnPosPaths

upSteps :: Int -> Path ()
upSteps = mk . flip L.replicate (UpStep ())

downSteps :: Int -> Path ()
downSteps = mk . flip L.replicate (DownStep ())

startUpStepPaths :: Int -> Int -> [Path ()]
startUpStepPaths k n = (upSteps k <>) <$> paths (n-k)

startDownStepPaths :: Int -> Int -> [Path ()]
startDownStepPaths k = fmap mirror . startUpStepPaths k

endUpStepPaths :: Int -> Int -> [Path ()]
endUpStepPaths k = fmap rev . startDownStepPaths k

endDownStepPaths :: Int -> Int -> [Path ()]
endDownStepPaths k = fmap rev . endUpStepPaths k

-- |'splitAtReturn' 'p'
splitAtReturn :: Path a -> (Path a, Path a)
splitAtReturn = (mk *** mk) . aux [] 0 . getSteps
  where
    aux acc h []
      | h == 0    = (L.reverse acc, [])
      | otherwise = ([], [])
    aux acc h (s@(UpStep x) : ss)
      | h == -1   = (L.reverse (s : acc), ss)
      | otherwise = aux (s : acc) (h+1) ss
    aux acc h (s@(DownStep x) : ss)
      | h == 1    = (L.reverse (s : acc), ss)
      | otherwise = aux (s : acc) (h-1) ss

-- splitEvery :: (a -> Bool) -> [a] -> [[a]]
splitEvery p = (\ (xs, xss) -> if L.null xs then xss else xs : xss) . F.foldr f ([], [])
  where
    f x (currentAcc, acc)
      | p x       = if L.null currentAcc then ([x], acc) else ([x], currentAcc : acc)
      | otherwise = (x : currentAcc, acc)
