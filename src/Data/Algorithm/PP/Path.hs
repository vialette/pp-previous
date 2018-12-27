module Data.Algorithm.PP.Path
(
  -- * Types
  Step(..)
, Path(..)

  -- * Constructing
, mk
, paths
, returnPaths
, returnPosPaths
, returnNegPaths
, startUpStepPaths
, startDownStepPaths
, strictlyPosPaths
, posPaths
, strictlyNegPaths
, negPaths

  -- * Transforming
, mirror
, rev
, splitAtReturn

  -- * Seeing
, showA

  -- * Querying
, len
, semiLen

  -- * Drawing
, draw
, draw'
)
where

  import Control.Arrow ((***))
  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Tuple    as T

  data Step a = UpStep a | DownStep a

  newtype Path a = Path { getSteps :: [Step a]}

  instance (Show a) => Show (Step a) where
    show (UpStep _)    = "("
    show (DownStep _)  = ")"

  instance (Show a) => Show (Path a) where
    show = F.concatMap show . getSteps

  -- |'showA' 'p'
  showA :: (Show a) => Path a -> String
  showA = F.concatMap f . getSteps
    where
      f (UpStep x)    = '(' : show x
      f (DownStep x)  = ')' : show x

  -- |'mk' 'ss'
  --
  -- >>>
  mk ss = Path { getSteps = ss }

  -- |'paths' 'n'
  --
  -- >>>
  paths :: Int -> [Path a]
  paths 0 = []
  paths n = fmap mk . aux n
    where
      aux 0 = []
      aux n' = fmap (UpStep () :) ss ++ fmap (DownStep () :) ss
        where
          ss = aux (n'-1)

  -- |'returnPaths' 'k' 'n' returns all Dyck paths of length 'n' with 'k' returns
  -- to the x-axis (exclusing the first and last points).
  --
  -- >>>
  returnPaths :: Int -> Int -> [Path a]
  returnPaths k n = ps


  -- |'returnNegPaths' 'k' 'n'
  --
  -- >>>
  returnNegPaths :: Int -> Int -> [Path a]
  returnNegPaths k = fmap mirror . returnPosPaths k

  -- |'startUpStepPaths' 'k' 'n'
  --
  -- >>>
  startUpStepPaths :: Int -> Int -> [Path a]
  startUpStepPaths k n = L.replicate k (UpStep ()) ++ paths (n-k)

  -- |'startDownStepPaths' 'k' 'n'
  --
  -- >>>
  startDownStepPaths :: Int -> Int -> [Path a]
  startDownStepPaths m n = L.replicate k (DownStep ()) ++ paths (n-k)

  -- |'strictlyPosPaths' 'n'
  --
  -- >>>
  strictlyPosPaths :: Int -> [Path a]
  strictlyPosPaths 0 = []
  strictlyPosPaths n = fmap mk . aux n 0
    where
      aux 0  _ = []
      aux n' h
        | h <= 1    = fmap (UpStep () :) (aux (n'-1) 1)
        | otherwise = fmap (UpStep () :) ss ++ fmap (DownStep () :) ss
          where
            ss = aux (n'-1) (h+1)

  -- |'posPaths' 'n'
  --
  -- >>>
  posPaths :: Int -> [Path a]
  posPaths 0 = []
  posPaths n = fmap mk . aux n 0
    where
      aux 0  _ = []
      aux n' h
        | h == 0    = fmap (UpStep () :) (aux (n'-1) 1)
        | otherwise = fmap (UpStep () :) ss ++ fmap (DownStep () :) ss
          where
            ss = aux (n'-1) (h+1)

  -- |'strictlyNegPaths' 'n'
  --
  -- >>>
  strictlyNegPaths :: Int -> [Path a]
  strictlyNegPaths = fmap mirror . strictlyPosPaths

  -- |'negPaths' 'n'
  --
  -- >>>
  negPaths :: Int -> [Path a]
  negPaths = fmap mirror . posPaths

  flipStep :: Step a -> Step a
  flipStep (UpStep x)   = DownStep x
  flipStep (DownStep x) = UpStep x

  -- |'mirror' 'path'
  --
  -- >>>
  mirror :: Path a -> Path a
  mirror = mk . fmap flipStep . getSteps

  -- |'rev' 'path' reverses the path 'path'.
  --
  -- >>>
  rev :: Path a -> Path a
  rev = mk . L.reverse . fmap flipStep . getSteps

  -- |'splitAtReturn' 'path'
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
        | otherwise = aux (s : acc) (h+1) ss


  -- |'len' 'path' return the length of the path 'path'.
  len :: Path a -> Int
  len = L.length . getSteps

  -- |'semiLen' 'path' return the semi-length of the path 'path'.
  semiLen :: Path a -> Int
  semiLen = flip div 2 . len

  locateSteps :: Path a -> (Int, [Step (Int, Int)])
  locateSteps = ((\ (_, _, h) -> h) *** L.reverse) . F.foldl f ((0, 0, 0), []) . getSteps
    where
      f ((x, y, maxY), ss) (UpStep _)   = ((x+1, y+1, max maxY y), UpStep   (x+1, y+1) : ss)
      f ((x, y, maxY), ss) (DownStep _) = ((x+1, y-1, max maxY y), DownStep (x+1, y)   : ss)

  layer :: Int -> [Step (Int, Int)] -> [Step (Int, Int)]
  layer y = F.foldr f []
    where
      f (s@(UpStep (_, y'))) acc
        | y == y'   = s : acc
        | otherwise = acc
      f (s@(DownStep (_, y'))) acc
        | y == y'   = s : acc
        | otherwise = acc

  drawLayer :: Char -> Char -> [Step (Int, Int)] -> String
  drawLayer cUp cDown = aux 0
    where
      aux _ []                   = "\n"
      aux x (UpStep   (x', _) : ss) = L.replicate (x'-x-1) ' ' ++ [cUp]   ++ aux x' ss
      aux x (DownStep (x', _) : ss) = L.replicate (x'-x-1) ' ' ++ [cDown] ++ aux x' ss

  -- Default UpStep charactr.
  defaultUpStepChar :: Char
  defaultUpStepChar = '/'

  -- Default DownStep charactr.
  defaultDownStepChar :: Char
  defaultDownStepChar = '\\'

  -- |'draw' 'p' stringify a path.
  --
  -- >>> mapM_ (putStr . (\ (i, p) -> show i ++ "\n" ++ show p ++ "\n" ++ draw p)) $ L.zip [1..] (paths 3)
  -- 1
  -- ()()()
  -- /\/\/\
  -- 2
  -- ()(())
  --    /\
  -- /\/  \
  -- 3
  -- (())()
  --  /\
  -- /  \/\
  -- 4
  -- (()())
  --  /\/\
  -- /    \
  -- 5
  -- ((()))
  --   /\
  --  /  \
  -- /    \
  draw :: Path a -> String
  draw = draw' defaultUpStepChar defaultDownStepChar

  -- |'draw'' 'p' stringify a path.
  --
  -- >>> mapM_ (putStr . (\ (i, p) -> show i ++ "\n" ++ show p ++ "\n" ++ draw' 'u' 'd' p)) $ L.zip [1..] (paths 3)
  -- 1
  -- ()()()
  -- ududud
  -- 2
  -- ()(())
  --    ud
  -- udu  d
  -- 3
  -- (())()
  --  ud
  -- u  dud
  -- 4
  -- (()())
  --  udud
  -- u    d
  -- 5
  -- ((()))
  --   ud
  --  u  d
  -- u    d
  draw' :: Char -> Char -> Path a -> String
  draw' cUp cDown p = F.concat [drawLayer cUp cDown (layer y ss) | y <- [maxY,maxY-1..1]]
    where
      (maxY, ss) = locateSteps p
