module Data.Algorithm.PP.Dyck
(
  -- * Types
  Step(..)
, LStep(..)
, Path(..)
, LPath(..)

  -- * Constructing
, mk
, paths

  -- * Seeing
, display

  -- * Querying
, len
, semiLen

  -- * Labeling
, labelLeftToRightDown

, draw
, draw'
, locateSteps
, drawLayer
, layer
)
where

  import Control.Arrow ((***))
  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Tuple    as T

  data LStep a = LUp a | LDown a

  newtype LPath a = LPath { getSteps :: [LStep a]}

  type Step = LStep ()

  type Path = LPath ()

  instance (Show a) => Show (LStep a) where
    show (LUp _)    = "("
    show (LDown _)  = ")"

  instance (Show a) => Show (LPath a) where
    show = F.concatMap show . getSteps

  -- |'display' 'p'
  display :: (Show a) => LPath a -> String
  display = F.concatMap f . getSteps
    where
      f (LUp x)    = '(' : show x
      f (LDown x)  = ')' : show x

  mk ss = LPath { getSteps = ss }

  -- |'paths' 'n' returns all Dyck path of semi-length 'n'.
  --
  -- >>> paths 0
  -- []
  -- >>> paths 1
  -- [()]
  -- >>> paths 2
  -- [()(),(())]
  -- >>> paths 3
  -- [()()(),()(()),(())(),(()()),((()))]
  paths :: Int -> [LPath ()]
  paths 0 = []
  paths n = LPath <$> aux n
    where
      aux 0  = [[]]
      aux n' = [[LUp ()] ++ xs ++ [LDown ()] ++ ys | m <- [0..n'-1], xs <- aux m, ys <- aux (n'-1-m)]

  -- |'len' 'p' return the length of the path 'p'.
  len :: LPath a -> Int
  len = L.length . getSteps

  -- |'semiLen' 'p' return the semi-length of the path 'p'.
  semiLen :: LPath a -> Int
  semiLen = flip div 2 . len

  -- |'leftToRightMinima' 'p'
  labelLeftToRightDown :: LPath a -> LPath Int
  labelLeftToRightDown p = LPath . T.fst . F.foldr g ([], []) . T.fst . F.foldr f ([], n) $ getSteps p
    where
      n = semiLen p

      f (LUp _)  (acc, n')  = (LUp 0 : acc, n')
      f (LDown _) (acc, n') = (LDown n' : acc, n'-1)

      g (LUp 0)   (acc, LDown i : s) = (LUp i : acc, s)
      g (LDown i) (acc, s)           = (LDown i : acc, LDown i : s)
      g _         _                  = error "non Dyck path"

  locateSteps :: LPath a -> (Int, [LStep (Int, Int)])
  locateSteps = ((\ (_, _, h) -> h) *** L.reverse) . F.foldl f ((0, 0, 0), []) . getSteps
    where
      f ((x, y, maxY), ss) (LUp _)   = ((x+1, y+1, max maxY y), LUp   (x+1, y+1) : ss)
      f ((x, y, maxY), ss) (LDown _) = ((x+1, y-1, max maxY y), LDown (x+1, y)   : ss)

  layer :: Int -> [LStep (Int, Int)] -> [LStep (Int, Int)]
  layer y = F.foldr f []
    where
      f (s@(LUp (_, y'))) acc
        | y == y'   = s : acc
        | otherwise = acc
      f (s@(LDown (_, y'))) acc
        | y == y'   = s : acc
        | otherwise = acc

  drawLayer :: Char -> Char -> [LStep (Int, Int)] -> String
  drawLayer cUp cDown = aux 0
    where
      aux _ []                   = "\n"
      aux x (LUp   (x', _) : ss) = L.replicate (x'-x-1) ' ' ++ [cUp]   ++ aux x' ss
      aux x (LDown (x', _) : ss) = L.replicate (x'-x-1) ' ' ++ [cDown] ++ aux x' ss

  defaultCUp :: Char
  defaultCUp = '/'

  defaultCDown :: Char
  defaultCDown = '\\'

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
  draw :: LPath a -> String
  draw = draw' defaultCUp defaultCDown

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
  draw' :: Char -> Char -> LPath a -> String
  draw' cUp cDown p = F.concat [drawLayer cUp cDown (layer y ss) | y <- [maxY,maxY-1..1]]
    where
      (maxY, ss) = locateSteps p
