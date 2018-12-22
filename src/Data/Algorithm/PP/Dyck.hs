module Data.Algorithm.PP.Dyck
(
  -- * Types
  Step(..)
, LStep(..)
, Path(..)
, LPath(..)

  -- * Constructing
, paths

  -- * Querying
, len
, semiLen

  -- * Labeling
, leftToRightDownLabel
)
where

  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Tuple    as T

  data Step = Up | Down

  data LStep a = LUp a | LDown a

  newtype Path = Path { getSteps :: [Step] }

  newtype LPath a = LPath { getLSteps :: [LStep a]}

  instance Show Step where
    show Up   = "("
    show Down = ")"

  instance (Show a) => Show (LStep a) where
    show (LUp x)   = '(' : show x
    show (LDown x) = ')' : show x

  instance Show Path where
    show = F.concatMap show . getSteps

  instance (Show a) => Show (LPath a) where
    show = F.concatMap show . getLSteps


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
  paths :: Int -> [Path]
  paths 0 = []
  paths n = Path <$> aux n
    where
      aux 0  = [[]]
      aux n' = [[Up] ++ xs ++ [Down] ++ ys | m <- [0..n'-1], xs <- aux m, ys <- aux (n'-1-m)]

  -- |'len' 'p' return the length of the path 'p'.
  len :: Path -> Int
  len = L.length . getSteps

  -- |'semiLen' 'p' return the semi-length of the path 'p'.
  semiLen :: Path -> Int
  semiLen = flip div 2 . len

  leftToRightDownLabel :: Path -> [LStep Int]
  leftToRightDownLabel p = T.fst . F.foldr g ([], []) . T.fst . F.foldr f ([], n) $ getSteps p
    where
      n = semiLen p

      f Up   (acc, n') = (LUp 0 : acc, n')
      f Down (acc, n') = (LDown n' : acc, n'-1)

      g (LUp 0)   (acc, LDown i : s) = (LUp i : acc, s)
      g (LDown i) (acc, s)           = (LDown i : acc, LDown i : s)
      g _         _                  = error "non Dyck path"
