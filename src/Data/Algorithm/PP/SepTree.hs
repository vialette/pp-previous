module Data.Algorithm.PP.SepTree
(
  SepTree
)
where

  import qualified Data.Algorithm.PP.Perm           as PP.Perm

  data SepTree = PlusNode SepTree SepTree
               | MinusNode SepTree SepTree
               | Leaf Int
               deriving (Show)

  -- |'mk' 'p' returns a separating tree of the permutation 'p' if it is separable.
  -- Otherwise, the functions returns 'Nothing'.
  mk :: PP.Perm.Perm -> Maybe SepTree
  mk = go [] . PP.Perm.getList
    where
      go s  [] = go' s
      go [] (x : xs) = go [(x, Leaf x, x)] xs
      go s'@((minX, t, maxX) : s) (x : xs)
        | x == maxX + 1 = go ((minX, PlusNode  t (Leaf x), x) : s)    xs
        | x == minX - 1 = go ((x,    MinusNode t (Leaf x), maxX) : s) xs
        | otherwise     = go ((x, Leaf x, x) : s')                    xs

      go' [] = Nothing
      go' [(_, t, _)] = Just t
      go' ((minX, t, maxX) : (minX', t', maxX') : s)
        | maxX  + 1 == minX' = go' ((minX,  PlusNode t t',  maxX') : s)
        | maxX' + 1 == minX  = go' ((minX', MinusNode t t', maxX) : s)
        | otherwise          = Nothing
