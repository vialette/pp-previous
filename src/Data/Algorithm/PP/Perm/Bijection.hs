module Data.Algorithm.PP.Perm.Bijection
(
  inv
, rev
, comp
, revComp
, compInv
, invComp
, invRevComp
)
where

  import qualified Data.List     as L
  import qualified Data.Foldable as F
  import qualified Data.Tuple    as T

  import Data.Algorithm.PP.Perm.Inner

  -- | 'inv p' returns the inverse of permutation 'p'.
  --
  -- >>> inv $ mk [1,3,4,2]
  -- [1,4,2,3]
  inv :: Perm -> Perm
  inv = mk . L.map T.snd . L.sort . flip L.zip [1..] . toList

  -- | 'rev' 'p' returns the reverse of permutation 'p'.
  --
  -- >>> rev $ mk [1,3,4,2]
  -- [2,4,3,1]
  rev :: Perm -> Perm
  rev = mk . L.reverse . toList

  -- | 'comp' 'p' returns the complement of permutation 'p'.
  --
  -- >>> comp $ mk [1,3,4,2]
  -- [4,2,1,3]
  comp :: Perm -> Perm
  comp p = mk $ fmap (\x -> m - x + 1) xs
    where
      xs = toList p
      m  = F.maximum xs

  -- | 'revComp' 'p' returns the reverse complement of permutation 'p'.
  --
  -- >>> revComp $ mk [1,3,4,2]
  -- [3,1,2,4]
  revComp :: Perm -> Perm
  revComp = rev . comp

  -- | 'compInv' 'p' returns the complement inverse of permutation 'p'.
  --
  -- >>> compInv $ mk [1,3,4,2]
  -- [4,1,3,2]
  compInv :: Perm -> Perm
  compInv = comp . inv

  -- | 'invComp' 'p' returns the inverse complement of permutation 'p'.
  --
  -- >>> invComp $ mk [1,3,4,2]
  -- [3,2,4,1]
  invComp :: Perm -> Perm
  invComp = inv . comp

  -- | 'invRevComp' 'p' returns the inverse reverse complement of permutation 'p'.
  --
  -- >>> invRevComp $ mk [1,3,4,2]
  -- [2,3,1,4
  invRevComp :: Perm -> Perm
  invRevComp = inv . rev . comp
