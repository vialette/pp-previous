module Data.Algorithm.PP.Utils.Maybe
(
  whenMaybe
)
where

  -- 
  whenMaybe :: Bool -> a -> Maybe a
  whenMaybe False _ = Nothing
  whenMaybe True  x = Just x
