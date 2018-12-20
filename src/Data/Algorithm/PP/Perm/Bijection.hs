module Data.Algorithm.PP.Perm.Bijection
(
  -- * Simion-Schmidt
  simionSchmidt
, invSimionSchmidt
)
where

  import qualified Data.Algorithm.PP.Perm as PP.Perm

  simionSchmidt :: PP.Perm.Perm -> PP.Perm.Perm
  simionSchmidt p = PP.Perm.fromPoints (ps' ++ ps'')
    where
      ps   = PP.Perm.getPoints p
      ps'  = PP.Perm.Statistics.leftToRightMinima p
      ps'' = L.zip [1..] . L.map T.fst . L.sortOn T.fst $ L.filter (not . `elem` ps') ps

  invSimionSchmidt :: PP.Perm.Perm -> PP.Perm.Perm
  invSimionSchmidt p = PP.Perm.fromPoints (ps' ++ ps'')
    where
      ps   = PP.Perm.getPoints p
      ps'  = PP.Perm.Statistics.leftToRightMinima p
      ps'' = L.zip [1..] . L.map T.fst . L.sortOn T.fst . L.filter (not . `elem` ps)
