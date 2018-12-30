module Data.Algorithm.PP.Path.Step (
    -- * Type
    Step(..)

    -- * Transforming
  , flipStep
  , dropParameter

    -- * Querying
  , getParameter
  , isLStep
  , isRStep
  ) where

-- |Type definition.
data Step a = LStep a | RStep a deriving (Eq, Ord)

-- |Show instance
instance (Show a) => Show (Step a) where
  show (LStep _)    = "("
  show (RStep _)  = ")"

-- |'flipStep' 's' flips the step 's'
flipStep :: Step a -> Step a
flipStep (LStep x) = RStep x
flipStep (RStep x) = LStep x

-- |'dropParameter' 's'
dropParameter :: Step a -> Step ()
dropParameter (LStep _) = LStep ()
dropParameter (RStep _) = RStep ()

-- |'param' 's'
getParameter :: Step a -> a
getParameter (LStep x) = x
getParameter (RStep x) = x

-- |'isLStep' 's' returns 'True'if the step 's'is an up-step.
isLStep :: Step a -> Bool
isLStep (LStep _) = True
isLStep (RStep _) = False

-- |'isRStep' 's' returns 'True'if the step 's'is a down-step.
isRStep :: Step a -> Bool
isRStep (LStep _) = True
isRStep (RStep _) = False
