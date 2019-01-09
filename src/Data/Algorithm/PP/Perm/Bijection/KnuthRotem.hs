module Data.Algorithm.PP.Perm.Bijection.KnuthRotem
(
  knuthRotem
, invKnuthRotem
)
where

  import Control.Applicative
  import qualified Data.Foldable as F
  import qualified Data.List     as L
  import qualified Data.Tuple    as T

  import qualified Data.Algorithm.PP.Dyck            as PP.Dyck
  import qualified Data.Algorithm.PP.Geometry.Point  as PP.Geometry.Point
  import qualified Data.Algorithm.PP.Perm            as PP.Perm
  import qualified Data.Algorithm.PP.Perm.Statistics as PP.Perm.Statistics
  import qualified Data.Algorithm.PP.Utils.List      as PP.Utils.List

  ballotSequence :: PP.Perm.Perm -> [Int]
  ballotSequence perm
    | PP.Perm.len perm == 1 = [0]
    | otherwise             = aux [0] (L.tail $ PP.Perm.getPoints perm) . L.tail $ PP.Perm.Statistics.leftToRightMaxima perm
    where
      aux s []       _  = L.reverse s
      aux s (p : ps) [] = aux (y : s)     ps []
        where
          y  = PP.Geometry.Point.getY p
      aux s (p : ps) (p' : ps')
        | x == x'    = aux (L.head s : s) ps ps'
        | otherwise  = aux (y : s)        ps (p' : ps')
          where
            x  = PP.Geometry.Point.getX p
            y  = PP.Geometry.Point.getY p
            x' = PP.Geometry.Point.getX p'

  buildCoordinates :: [Int] -> [(Int, Int)]
  buildCoordinates = (:) (0, 0) . F.foldr f [] . L.zip [1..]
    where
      f (x, y) [] = [(x, y), (x, x)]
      f (x, y) acc'@((x', y') : acc)
        | y /= y'   = [(x, y'') | y'' <- [y..y']] ++ acc'
        | otherwise = (x, y) : acc'

  buildPath :: [(Int, Int)] -> PP.Dyck.Path
  buildPath = PP.Dyck.mkUnsafe . fmap f . PP.Utils.List.chunk2 . fmap (uncurry (-)) . L.reverse
    where
      f (y, y')
        | y < y'    = PP.Dyck.UpStep
        | otherwise = PP.Dyck.DownStep

  -- |'knuthRotem' 'perm'
  --
  -- >>> knuthRotem (mkPerm [2,5,1,3,4,7,6])
  -- ()(())()(()())
  -- >>> putStr . draw $ knuth (mkPerm [2,5,1,3,4,7,6])
  --    /\    /\/\
  -- /\/  \/\/    \
  -- >>> putStr . draw $ knuth (mkPerm [7,5,6,4,2,1,3])
  --    /\    /\/\
  -- /\/  \/\/    \
  knuthRotem :: PP.Perm.Perm -> PP.Dyck.Path
  knuthRotem = buildPath . buildCoordinates . ballotSequence

  -- |'invKnuthRotem' 'perm'
  invKnuthRotem :: PP.Perm.Perm -> PP.Perm.Perm
  invKnuthRotem _ = PP.Perm.mkPerm [1]
