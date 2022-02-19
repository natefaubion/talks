module Rec11 where

import Prelude

import Data.Either (Either(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Mu (Mu, roll, unroll)

type DelayedMu f = Mu (Compose (Function Unit) f)

fix :: forall b. DelayedMu (Either b) -> b
fix mu = case unroll mu of
  Compose thunk ->
    case thunk unit of
      Left result ->
        result
      Right mu' ->
        fix mu'

data Bin a = Tip | Branch (Bin a) a (Bin a)

data MapAccum a b
  = ContLhs (a -> b) a (Bin a) (MapAccum a b)
  | ContRhs (Bin b) b (MapAccum a b)
  | ContIdentity

mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
mapBin = (\f bin -> fix (go f bin ContIdentity))
  where
  go f bin cont = roll $ Compose $ \_ -> case bin of
    Tip ->
      eval cont Tip
    Branch l a r ->
      Right (go f l (ContLhs f a r cont))

  eval cont bin = case cont of
    ContLhs f a r next -> do
      let a' = f a
      Right (go f r (ContRhs bin a' next))
    ContRhs l' a' next ->
      eval next (Branch l' a' bin)
    ContIdentity ->
      Left bin

