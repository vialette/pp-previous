module Data.Algorithm.PP.Stack
  (
    -- * Type
      Stack(..)

    -- * Constructing
    , mk

    -- * Modifying
    , popUnsafe
    , pop
    , push

    -- * Querying
    , topUnsafe
    , top
    , empty
  ) where

import qualified Data.Algorithm.PP.Utils.List.Safe as PP.Utils.List.Safe

type Stack a = [a]

mk :: Stack a
mk = []

popUnsafe :: Stack a -> Stack a
popUnsafe []       = error "empty stack"
popUnsafe (_ : xs) = xs

pop :: Stack a -> Maybe (Stack a)
pop = PP.Utils.List.Safe.tail

push :: a -> Stack a -> Stack a
push x xs = x : xs

topUnsafe :: Stack a -> a
topUnsafe [] = error "emty stack"
topUnsafe (x : _) = x

top :: Stack a -> Maybe a
top = PP.Utils.List.Safe.head

empty :: Stack a -> Bool
empty [] = True
empty _  = False