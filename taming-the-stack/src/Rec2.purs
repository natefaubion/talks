module Rec2 where

data Bin a = Tip | Branch (Bin a) a (Bin a)

mapBin :: forall a b. (a -> b) -> Bin a -> Bin b
mapBin f = case _ of
  Tip ->
    Tip
  Branch l a r -> do
    let l' = mapBin f l
    let a' = f a
    let r' = mapBin f r
    Branch l' a' r'
