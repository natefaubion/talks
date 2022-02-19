module Rec7 where

import Control.Monad.Rec.Class (Step(..), tailRec)

data Bin a = Tip | Branch (Bin a) a (Bin a)

data MapAccum a b
  = ContLhs (a -> b) a (Bin a) (MapAccum a b)
  | ContRhs (Bin b) b (MapAccum a b)
  | ContIdentity

data MapCall a b
  = MapGo (a -> b) (Bin a) (MapAccum a b)
  | MapEval (MapAccum a b) (Bin b)

mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
mapBin = (\f bin -> tailRec go (MapGo f bin ContIdentity))
  where
  go call = case call of
    MapGo f bin cont -> case bin of
      Tip ->
        Loop (MapEval cont Tip)
      Branch l a r ->
        Loop (MapGo f l (ContLhs f a r cont))
    MapEval cont bin -> case cont of
      ContLhs f a r next -> do
        let a' = f a
        Loop (MapGo f r (ContRhs bin a' next))
      ContRhs l' a' next ->
        Loop (MapEval next (Branch l' a' bin))
      ContIdentity ->
        Done bin

