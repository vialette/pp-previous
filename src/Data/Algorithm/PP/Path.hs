module Data.Algorithm.PP.Path
(
  -- * Types
  Step(..)

  -- *
, module Data.Algorithm.PP.Path.Step

  -- * Constructing
, mk
, paths
, returnPaths
, noReturnPaths
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
, showFull

  -- * Querying
, len
, semiLen

  -- * Drawing
, draw
, draw'
)
where

  import Control.Applicative ((<$>), (<*>))
  import Control.Arrow ((***))
  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Tuple    as T

  import Data.Algorithm.PP.Path.Step
  import qualified Data.Algorithm.PP.Geometry.Point as PP.Geometry.Point

  newtype Path a = Path { getSteps :: [Step a] } deriving (Eq, Ord)

  data Render = Horizontal | Diagonal

  instance Semigroup (Path a) where
    p <> p' = mk (getSteps p ++ getSteps p')

  instance Monoid (Path a) where
      mempty = mk []

  instance (Show a) => Show (Path a) where
    show = F.concatMap show . getSteps

  -- |'showFull' 'p'
  showFull :: (Show a) => Path a -> String
  showFull = F.concatMap f . getSteps
    where
      f (UpStep x)    = '(' : show x
      f (DownStep x)  = ')' : show x

  -- |'mk' 'ss' return a path from a list of steps 'ss'.
  --
  -- >>>
  mk :: [Step a] -> Path a
  mk ss = Path { getSteps = ss }

  -- |'len' 'path' return the length of the path 'path'.
  len :: Path a -> Int
  len = L.length . getSteps

  -- |'semiLen' 'path' return the semi-length of the path 'path'.
  semiLen :: Path a -> Int
  semiLen = flip div 2 . len

  -- |'flipStep' 's' flips the step 's'
  flipStep :: Step a -> Step a
  flipStep (UpStep x)   = DownStep x
  flipStep (DownStep x) = UpStep x

  upSteps :: Int -> Path ()
  upSteps = mk . flip L.replicate (UpStep ())

  downSteps :: Int -> Path ()
  downSteps = mk . flip L.replicate (DownStep ())

  -- |'rev' 'p' reverses the path 'p'.
  --
  -- >>>
  rev :: Path a -> Path a
  rev = mk . L.reverse . fmap flipStep . getSteps

  -- |'mirror' 'p' returns the mirror of the path 'p'.
  --
  -- >>>
  mirror :: Path a -> Path a
  mirror = mk . fmap flipStep . getSteps

  -- |'map' 'f' 'p'
  --
  -- >>>
  map :: (Step a -> Step b) -> Path a -> Path b
  map f = mk . fmap f . getSteps

  -- |'paths' 'n' returns all paths of length 'n' starting from the x-axis.
  --
  -- >>>
  paths :: Int -> [Path ()]
  paths 0 = []
  paths n = mk <$> aux n
    where
      aux :: Int -> [[Step ()]]
      aux 0 = [[]]
      aux n' = (:) <$> [UpStep (), DownStep ()] <*> aux (n'-1)

  -- Auxialiary function
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

  -- | 'strictlyPosPaths' 'n' returns all strictly positive paths of length 'n'
  -- starting from the x-axis with an up-step .
  --
  -- >>>
  strictlyPosPaths :: Int -> [Path ()]
  strictlyPosPaths = predicatePosPaths (<= 1)

  -- | 'posPaths' 'n' returns all positive paths of length 'n' starting from the
  -- x-axis with an up-step .
  --
  -- >>>
  posPaths :: Int -> [Path ()]
  posPaths = predicatePosPaths (== 1)

  -- |'returnPosPaths' 'k' 'n' return all positive paths of length 'n' starting from and
  -- ending at the x-axis with 'k' internal returns to the x-axis.
  --
  -- >>>
  returnPosPaths :: Int -> Int -> [Path ()]
  returnPosPaths k n = F.concatMap f $ evenPartitions (k+1) n
      where
        f :: [Int] -> [Path ()]
        f xs = (mk . mconcat) <$> sequence [[[UpStep ()] `mappend` ss `mappend` [DownStep ()] | ss <- aux (x-2)] | x <- xs]

        aux :: Int-> [[Step ()]]
        aux 0  = [[]]
        aux n' = [[UpStep ()] `mappend` ss `mappend` [DownStep ()] `mappend` ss' | m <- [0..n'-2]
                                                                                 , ss  <- aux m
                                                                                 , ss' <- aux (n'-2-m)]

  -- |'anyReturnPosPaths' 'n' return all positive paths of length 'n' starting from and
  -- ending at the x-axis with no constraint on the number of internal returns to the x-axis.
  --
  -- >>>
  anyReturnPosPaths :: Int -> [Path ()]
  anyReturnPosPaths n = F.concat [returnPosPaths k n | k <- [0..n `div`2]]

  -- | 'strictlyNegPaths' 'n' returns all strictly negative paths of length 'n'
  -- starting from the x-axis with an up-step .
  --
  -- >>>
  strictlyNegPaths :: Int -> [Path ()]
  strictlyNegPaths = fmap mirror . strictlyNegPaths

  -- | 'negPaths' 'n' returns all negative paths of length 'n' starting from the
  -- x-axis with an up-step .
  --
  -- >>>
  negPaths :: Int -> [Path ()]
  negPaths = fmap mirror . posPaths

  -- |'returnNegPaths' 'k' 'n' return all negative paths of length 'n' starting from and
  -- ending at the x-axis with 'k' internal returns to the x-axis.
  --
  -- >>>
  returnNegPaths :: Int -> Int -> [Path ()]
  returnNegPaths k = fmap mirror . returnPosPaths k

  -- |'anyReturnNegPaths' 'n' return all negative paths of length 'n' starting from and
  -- ending at the x-axis with no constraint on the number of internal returns to the x-axis.
  --
  -- >>>
  anyReturnNegPaths :: Int -> [Path ()]
  anyReturnNegPaths = fmap mirror . anyReturnPosPaths

  -- |'startUpStepPaths' 'k' 'n' returns all paths of length 'n' that start from
  -- the x-axis with 'k' up-steps.
  --
  -- >>>
  startUpStepPaths :: Int -> Int -> [Path ()]
  startUpStepPaths k n = (upSteps k <>) <$> paths (n-k)

  -- |'startDownStepPaths' 'k' 'n' returns all paths of length 'n' that start from
  -- the x-axis with 'k' down-steps.
  --
  -- >>>
  startDownStepPaths :: Int -> Int -> [Path ()]
  startDownStepPaths k = fmap mirror . startUpStepPaths k

  -- |'endUpStepPaths' 'k' 'n' returns all paths of length 'n' that end at
  -- the x-axis with 'k' up-steps.
  --
  -- >>>
  endUpStepPaths :: Int -> Int -> [Path ()]
  endUpStepPaths k = fmap rev . startDownStepPaths k

  -- |'endDownStepPaths' 'k' 'n' returns all paths of length 'n' that end at
  -- the x-axis with 'k' down-steps.
  --
  -- >>>
  endDownStepPaths :: Int -> Int -> [Path ()]
  endDownStepPaths k = fmap rev . endUpStepPaths k

  -- |'splitAtReturn' 'p' return a pair @(p', p'')@ of paths such that 'p'' is
  -- prefix of the path 'p' that return to the x-axis and 'p''' is the remaining
  -- suffix.
  --
  -- >>>
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

  -- getPoints auxialiary function (horizontal render).
  getHorizontalRenderPoints :: Path a -> Path (a, PP.Geometry.Point.Point)
  getHorizontalRenderPoints = mk . L.reverse . T.snd . F.foldl f (PP.Geometry.Point.zero, []) . getSteps
      where
        f (p, acc) (UpStep x) = (p', UpStep (x, p') : acc)
          where
            p' = PP.Geometry.Point.mv (+1) (+1) p
        f (p, acc) (DownStep x) = ((p', UpStep (x, p'') : acc)
          where
            p'  = PP.Geometry.Point.mv (+1) (-1) p
            p'' = PP.Geometry.Point.mv (+1) 0    p

  -- getPoints auxialiary function (diagonal render).
  getDiagonalRenderPoints :: Path a -> Path (a, PP.Geometry.Point.Point)
  getDiagonalRenderPoints = mk . L.reverse . T.snd . F.foldl f (PP.Geometry.Point.zero, []) . getSteps
      where
        f (p, acc) (UpStep x) = (p', UpStep (x, p') : acc)
          where
            p' = PP.Geometry.Point.mv 0 (+1) p
        f (p, acc) (DownStep x) = ((p', UpStep (x, p'') : acc)
          where
            p'  = PP.Geometry.Point.mv (+1) 0 p

  -- |'getPoints' 'r' 'p' associates to the path 'p' the corresponding list of points
  -- according to the rendering 'r' ('Horizontal' or 'Diagonal').
  getPoints :: Render -> Path a -> Path (a, PP.Geometry.Point.Point)
  getPoints Horizontal = getHorizontalRenderPoints
  getPoints Diagonal   = getDiagonalRenderPoints

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
