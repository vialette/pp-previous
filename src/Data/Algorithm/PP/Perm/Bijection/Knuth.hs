module Data.Algorithm.PP.Perm.Bijection.Knuth
(
  knuth132AvoidingPermTo321AvoidingPerm
, knuth321AvoidingPermTo132AvoidingPerm
)
where

  import Control.Applicative
  import qualified Data.Foldable  as F
  import qualified Data.List      as L
  import qualified Data.Tuple     as T

  import qualified Data.Algorithm.PP.Dyck            as PP.Dyck
  import qualified Data.Algorithm.PP.Perm            as PP.Perm
  import qualified Data.Algorithm.PP.Utils.List      as PP.Utils.List

  -- 'knuth132AvoidingPermToDyckPath' 'perm'
  -- bijection from
  -- 132-avoiding permutations to Dyck paths.
  --
  -- >>> knuth (mkPerm [7,5,6,4,2,1,3])
  -- ()(())()(()())
  knuth132AvoidingPermToDyckPath :: PP.Perm.Perm -> PP.Dyck.LPath ()
  knuth132AvoidingPermToDyckPath = PP.Dyck.mk . aux . PP.Perm.getList
    where
      aux []  = []
      aux [_] = [PP.Dyck.LUp (), PP.Dyck.LDown ()]
      aux xs  = [PP.Dyck.LUp ()] ++ aux left ++ [PP.Dyck.LDown ()] ++ aux right
        where
          maxY = F.maximum xs
          (left, right) = PP.Utils.List.splitOn maxY xs

  -- |'knuthDyckPathTo132AvoidingPerm' 'path'
  knuthDyckPathTo132AvoidingPerm :: PP.Dyck.LPath a -> PP.Perm.Perm
  knuthDyckPathTo132AvoidingPerm _ = PP.Perm.mkPerm [1]

  -- |'knuth321AvoidingPermToDyckPath' 'perm'
  knuth321AvoidingPermToDyckPath :: PP.Perm.Perm -> PP.Dyck.LPath ()
  knuth321AvoidingPermToDyckPath perm = PP.Dyck.mk (left ++ L.reverse right)
    where
      (pTableau, qTableau) = PP.StdYoungTableau.robinsonSchensted perm

      ys = PP.Perm.getList perm

      firstRowPTableau = L.head $ PP.StdYoungTableau.getRows pTableau
      left = F.Foldr f [] ys
        where
          f y acc
            | y `L.elem` firstRowPTableau = PP.Dyck.LUp ()   : acc
            | otherwise                   = PP.Dyck.LDown () : acc

      firstRowQTableau = L.head $ PP.StdYoungTableau.getRows pTableau
      right = F.Foldr f [] ys
        where
          f y acc
            | y `L.elem` firstRowQTableau = PP.Dyck.LDown () : acc
            | otherwise                   = PP.Dyck.LUp ()   : acc

  -- |'knuthDyckPathTo321AvoidingPerm' 'path'
  knuthDyckPathTo321AvoidingPerm :: PP.Dyck.LPath a -> PP.Perm.Perm
  knuthDyckPathTo321AvoidingPerm = PP.Perm.mkPerm [1]

  -- |'knuth132AvoidingPermTo321AvoidingPerm' 'perm'
  knuth132AvoidingPermTo321AvoidingPerm :: PP.Perm.Perm -> PP.Perm.Perm
  knuth132AvoidingPermTo321AvoidingPerm = knuthDyckPathTo321AvoidingPerm . knuth132AvoidingPermToDyckPath

  -- |'knuth321AvoidingPermTo132AvoidingPerm' 'perm'
  knuth321AvoidingPermTo132AvoidingPerm :: PP.Perm.Perm -> PP.Perm.Perm
  knuth321AvoidingPermTo132AvoidingPerm = knuthDyckPathTo132AvoidingPerm . knuth321AvoidingPermToDyckPath
