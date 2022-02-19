module Rec15 where

import Prelude

import Control.Monad.Trampoline (Trampoline, runTrampoline, delay)

data Bin a = Tip | Branch (Bin a) a (Bin a)

suspend :: forall a. (Unit -> Trampoline a) -> Trampoline a
suspend = join <<< delay

mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
mapBin = (\f bin -> runTrampoline (go f bin))
  where
  go f bin = suspend \_ -> case bin of
    Tip ->
      pure Tip
    Branch l a r ->
      Branch <$> go f l <*> pure (f a) <*> go f r
