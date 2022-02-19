module Rec10 where

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

data MapCall a b
  = MapGo (a -> b) (Bin a) (MapAccum a b)
  | MapEval (MapAccum a b) (Bin b)

mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
mapBin = (\f bin -> fix (go (MapGo f bin ContIdentity)))
  where
  go call = roll $ Compose $ \_ -> case call of
    MapGo f bin next ->
      case bin of
        Tip ->
          Right (go (MapEval next Tip))
        Branch l a r ->
          Right (go (MapGo f l (ContLhs f a r next)))
    MapEval cont bin ->
      case cont of
        ContLhs f a r next -> do
          let a' = f a
          Right (go (MapGo f r (ContRhs bin a' next)))
        ContRhs l' a' next ->
          Right (go (MapEval next (Branch l' a' bin)))
        ContIdentity ->
          Left bin

