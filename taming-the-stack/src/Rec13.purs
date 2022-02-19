module Rec13 where

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

type MapCall b = DelayedMu (Either (Bin b))

type MapAccum b = Bin b -> DelayedMu (Either (Bin b))

mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
mapBin = (\f bin -> fix (go f bin done))
  where
  go :: (a -> b) -> Bin a -> MapAccum b -> MapCall b
  go f bin cont = roll $ Compose $ \_ -> Right case bin of
    Tip ->
      cont Tip
    Branch l a r ->
      go f l \l' -> do
        let a' = f a
        go f r \r' ->
          cont (Branch l' a' r')

  done :: MapAccum b
  done = roll <<< Compose <<< const <<< Left

