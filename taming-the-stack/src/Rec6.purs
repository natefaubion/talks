module Rec6 where

data Bin a = Tip | Branch (Bin a) a (Bin a)

data MapAccum a b
  = ContLhs (a -> b) a (Bin a) (MapAccum a b)
  | ContRhs (Bin b) b (MapAccum a b)
  | ContIdentity

data MapCall a b
  = MapGo (a -> b) (Bin a) (MapAccum a b)
  | MapEval (MapAccum a b) (Bin b)

mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
mapBin = (\f bin -> go (MapGo f bin ContIdentity))
  where
  go call = case call of
    MapGo f bin cont -> case bin of
      Tip ->
        go (MapEval cont Tip)
      Branch l a r ->
        go (MapGo f l (ContLhs f a r cont))
    MapEval cont bin -> case cont of
      ContLhs f a r next -> do
        let a' = f a
        go (MapGo f r (ContRhs bin a' next))
      ContRhs l' a' next ->
        go (MapEval next (Branch l' a' bin))
      ContIdentity ->
        bin

