module Data.Algorithm.PP.Utils.List
(
  uniq
)
where

  import qualified Data.Set as S

  -- 'uniq xs' removes duplicates in 'xs'.
  uniq :: Ord a => [a] -> [a]
  uniq = S.toList . S.fromList
