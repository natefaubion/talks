module Rec3 where

import Prelude

data Bin a = Tip | Branch (Bin a) a (Bin a)

mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
mapBin = (\f bin -> go f bin identity) -- Identity cont.
  where
  go f bin next = case bin of
    Tip ->
      next Tip
    Branch l a r ->
      go f l \l' -> do -- Lhs cont.
        let a' = f a
        go f r \r' -> -- Rhs cont.
          next (Branch l' a' r')
