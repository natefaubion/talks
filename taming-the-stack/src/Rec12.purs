module Rec12 where

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
mapBin = (\f bin -> fix (go f bin contId))
  where
  go :: (a -> b) -> Bin a -> MapAccum b -> MapCall b
  go f bin cont = roll $ Compose $ \_ -> case bin of
    Tip ->
      Right (cont Tip)
    Branch l a r ->
      Right (go f l (contLhs f a r cont))

  contId :: MapAccum b
  contId = roll <<< Compose <<< const <<< Left

  contLhs :: (a -> b) -> a -> Bin a -> MapAccum b -> MapAccum b
  contLhs f a r cont = \l' -> do
    let a' = f a
    go f r (contRhs l' a' cont)

  contRhs :: Bin b -> b -> MapAccum b -> MapAccum b
  contRhs l' a' cont = \r' ->
    cont (Branch l' a' r')

