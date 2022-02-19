module Rec8 where

data Bin a = Tip | Branch (Bin a) a (Bin a)

data MapAccum a b
  = ContLhs (a -> b) a (Bin a) (MapAccum a b)
  | ContRhs (Bin b) b (MapAccum a b)
  | ContIdentity

data MapCall a b
  = MapGo (a -> b) (Bin a) (MapAccum a b)
  | MapEval (MapAccum a b) (Bin b)

step :: forall a b. MapCall a b -> MapCall a b
step call = case call of
  MapGo f bin cont -> case bin of
    Tip ->
      MapEval cont Tip
    Branch l a r ->
      MapGo f l (ContLhs f a r cont)
  MapEval cont bin -> case cont of
    ContLhs f a r next -> do
      let a' = f a
      MapGo f r (ContRhs bin a' next)
    ContRhs l' a' next ->
      MapEval next (Branch l' a' bin)
    ContIdentity ->
      MapEval ContIdentity bin

eval :: forall a b. MapCall a b -> Bin b
eval call = case step call of
  MapEval ContIdentity result ->
    result
  next ->
    eval next

mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
mapBin f a = eval (MapGo f a ContIdentity)
