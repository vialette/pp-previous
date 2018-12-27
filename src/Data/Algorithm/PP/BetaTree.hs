module Data.Algorithm.PP.BetaTree
(

)
where
  --
  -- data Zero = Zero deriving Show
  -- data Succ a = Succ a deriving Show
  --
  -- class Card c where
  --     cardNum :: (Num a) => c -> a -- convert to a number
  --
  -- cpred :: Succ c -> c
  -- cpred = undefined
  --
  -- instance Card Zero where
  --     cardNum _ = 0
  --
  -- instance (Card c) => Card (Succ c) where
  --     cardNum x = 1 + cardNum (cpred x)
  --
  -- data BetaTree l r = BetaBranch Int [BetaTree l r] | BetaLeaf Int
  --
  -- getLabel :: BetaTree l r -> Int
  -- getLabel (BetaLeaf x) = x
  -- getLabel (BetaBranch x _) = x
  --
  -- valid :: (Card l, Card r) => BetaTree l r -> Bool
  -- valid (BetaLeaf x) = x == cardNum l
  -- valid (BetaBranch x ts) = x <= cardNum r + sum [getLabel t | t <- ts]
