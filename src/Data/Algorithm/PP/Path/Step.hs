module Data.Algorithm.PP.Path.Step
(

)
where

  data Step a = UpStep a | DownStep a deriving (Eq, Ord)

  instance (Show a) => Show (Step a) where
    show (UpStep _)    = "("
    show (DownStep _)  = ")"

  -- Default UpStep charactr.
  defaultUpStepChar :: Char
  defaultUpStepChar = '/'

  -- Default DownStep charactr.
  defaultDownStepChar :: Char
  defaultDownStepChar = '\\'

  -- Default UpStep charactr.
  defaultUpStepChar :: Char
  defaultUpStepChar = '|'

  -- Default DownStep charactr.
  defaultRightStepChar :: Char
  defaultRightStepChar = '-'
