module Data.Algorithm.PP.Path.DyckPath.Generator (
  -- * Basic paths
    paths

  -- * Height
  , heightPaths

  -- *
  , returnPaths

  ) where

import qualified Data.Foldable as F


import qualified Data.Algorithm.PP.Path           as PP.Path
import qualified Data.Algorithm.PP.Path.Generator as PP.Path.Generator
import qualified Data.Algorithm.PP.Combinatorics  as PP.Combinatorics

{- |'paths' @n@ returns all Dyck paths of length @n@.

>>> paths 0
[]
>>> paths 1
[]
>>> paths 6
[(()()),((())),()(()),(())(),()()()]
-}
paths :: Int -> [PP.Path.Path]
paths =  PP.Path.Generator.positivePaths

{- | 'heightPaths' @h@ @n@ generates all Dyck paths of length @n@ and height at most @h@.

>>>
-}
heightPaths :: Int -> Int -> [PP.Path.Path]
heightPaths h n = F.concat [exactHeightPaths h' n | h' <- [1..h]]

{- | 'exactHeightPaths'@h@ @n@ generates all Dyck paths of length @n@ and height exactly @h@.

>>>
-}
exactHeightPaths :: Int -> Int -> [PP.Path.Path]
exactHeightPaths h n = []

{- | 'returnPaths' @k@ @n@ generates all Dyck paths of length @n@ with @k@ returns to 0.

>>>
-}
returnPaths :: Int -> Int -> [PP.Path.Path]
returnPaths = PP.Path.Generator.returnPositivePaths

