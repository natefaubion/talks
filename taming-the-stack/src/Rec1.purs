module Rec1 where

data Bin a = Tip | Branch (Bin a) a (Bin a)

mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
mapBin f = case _ of
  Tip ->
    Tip
  Branch l a r ->
    Branch (mapBin f l) (f a) (mapBin f r)
