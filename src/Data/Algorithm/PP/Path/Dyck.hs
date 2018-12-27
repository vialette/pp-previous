module Data.Algorithm.PP.Path.Dyck
(
  -- * Constructing
  mk

  -- * Generating
, paths
, returnPaths
, noReturnPaths
, startUpStepPaths
, endDownStepPaths
, peakPaths
, valleyPaths
, heighPaths

  -- * Tranforming
, collapse

  -- * Labeling
, labelLeftToRightDown
)
where

  import qualified Data.List.Split as L.Split

  import qualified Data.Algorithm.PP.Path as PP.Path

  -- |'mk' 'steps'
  mk :: [PP.Path.Step a] -> Maybe (PP.Path.Path ())
  mk = PP.Path.mk . aux 0 0
    where
      aux upCounter downCounter [] = if upCounter == 0 && downCounter == 0 then Just (PP.Path.mk ss) else Nothing
      aux upCounter downCounter (PP.Path.UpStep : ss)
        | upCounter >= downCounter = aux (upCounter+1) downCounter ss
        | otherwise                = Nothing
      aux upCounter downCounter (s : ss)
        | upCounter >= downCounter = aux (upCounter+1) downCounter ss
        | otherwise                = Nothing

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
  paths :: Int -> [PP.Path.Path ()]
  paths = fmap PP.Path.mk . aux
    where
      aux 0 = [[]]
      aux n = [[PP.Path.UpStep ()] ++ xs ++ [PP.Path.DownStep ()] ++ ys | m <- [0..n-1]
                                                                        , xs <- aux m, ys <- aux (n-1-m)]

  -- |'returnPaths' 'k' 'n' returns all Dyck paths of length 'n' with 'k' returns
  -- to the x-axis (excluding the first and last steps).
  returnPaths :: Int -> Int -> [Path a]
  returnPaths k n = ps

  -- |'noReturnPaths' 'n' returns all Dyck path with no return to the x-axis
  -- (excluding the first and last steps).
  --
  -- >>>
  noReturnPaths :: Int -> [Path a]
  noReturnPaths = returnPaths 0

  -- |'startUpStepPaths' 'k' 'n' returns all Dyck path of length 'n' that start
  -- with 'k' up-steps.
  --
  -- >>>
  startUpStepPaths :: Int -> Int -> [Path a]
  startUpStepPaths k n = L.replicate k (UpStep ()) ++ paths (n-k)

  -- |'endDownStepPaths' 'k' 'n' returns all Dyck path of length 'n' that end
  -- with 'k' down-steps.
  --
  -- >>>
  endDownStepPaths :: Int -> Int -> [Path a]
  endDownStepPaths k = fmap PP.Path.mirror . startUpStepPaths k

  -- |'peakPaths' 'k' 'n' returns all Dyck paths of length 'n' with 'k' peaks
  -- (an up-step preceding a down-step).
  --
  -- >>>
  peakPaths :: Int -> Int -> [Path a]
  peakPaths k n = []

  -- |'peakPaths' 'k' 'n' returns all Dyck paths of length 'n' with 'k' valleys
  -- (a down-step preceding an up-step).
  --
  -- >>>
  valleyPaths :: Int -> Int -> [Path a]
  valleyPaths k n = []

  -- |'heighPaths' 'h' 'n' returns all Dyck paths of length 'n' with maximum
  -- height 'h'.
  --
  -- >>>
  heighPaths :: Int -> Int -> [Path a]
  heighPaths h n = []

  -- |'collapse' 'i' 'path'
  --
  -- >>>
  collapse :: Int -> PP.Path.Path a -> [PP.Path.Path a]
  collapse i = fmap (fmap T.fst) . L.Split.splitWhen (flip (<=) i . T.snd) . F.foldr level ([], 0) . PP.Path.getSteps
    where
      level s@(PP.Path.UpStep _)   (acc, h) = (s : acc, h+1)
      level s@(PP.Path.DownStep _) (acc, h) = (s : acc, h-1)

  -- |'leftToRightMinima' 'p'
  --
  -- >>>
  labelLeftToRightDown :: Path a -> PP.Path.Path Int
  labelLeftToRightDown p = Path . T.fst . F.foldr g ([], []) . T.fst . F.foldr f ([], n) $ getSteps p
    where
      n = semiLen p

      f (UpStep _)   (acc, n') = (UpStep 0 : acc, n')
      f (DownStep _) (acc, n') = (DownStep n' : acc, n'-1)

      g (UpStep 0)   (acc, DownStep i : s) = (UpStep i : acc, s)
      g (DownStep i) (acc, s)              = (DownStep i : acc, DownStep i : s)
      g _            _                     = error "non Dyck path"
