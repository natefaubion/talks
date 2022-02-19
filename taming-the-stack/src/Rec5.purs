module Rec5 where

data Bin a = Tip | Branch (Bin a) a (Bin a)

data MapAccum a b
  = ContLhs (a -> b) a (Bin a) (MapAccum a b)
  | ContRhs (Bin b) b (MapAccum a b)
  | ContIdentity

mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
mapBin = (\f bin -> go f bin ContIdentity)
  where
  go f bin cont = case bin of
    Tip ->
      eval cont Tip
    Branch l a r ->
      go f l (ContLhs f a r cont)

  eval cont bin = case cont of
    ContLhs f a r next -> do
      let a' = f a
      go f r (ContRhs bin a' next)
    ContRhs l' a' next ->
      eval next (Branch l' a' bin)
    ContIdentity ->
      bin

