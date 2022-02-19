module Rec4 where

import Prelude

data Bin a = Tip | Branch (Bin a) a (Bin a)

mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
mapBin = (\f bin -> go f bin identity) -- Identity cont.
  where
  go f bin cont = case bin of
    Tip ->
      cont Tip
    Branch l a r ->
      go f l (contLhs f a r cont)

  contLhs f a r cont = \l' -> do -- Lhs cont.
    let a' = f a
    go f r (contRhs l' a' cont)

  contRhs l' a' cont = \r' -> -- Rhs cont.
    cont (Branch l' a' r')
