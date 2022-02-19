module Rec9 where

import Prelude

import Data.Either (Either(..))
import Data.Functor.Nu (Nu, observe, unfold)

fix :: forall b. Nu (Either b) -> b
fix nu = case observe nu of
  Left ret ->
    ret
  Right nu' ->
    fix nu'

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

eval' :: forall a b. MapCall a b -> Nu (Either (Bin b))
eval' = (\call -> unfold call (check <<< step))
  where
  check = case _ of
    MapEval ContIdentity result ->
      Left result
    next ->
      Right next

eval :: forall a b. MapCall a b -> Bin b
eval = fix <<< eval'

mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
mapBin f a = eval (MapGo f a ContIdentity)
