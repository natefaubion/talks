module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Rec1 as Rec1
import Rec10 as Rec10
import Rec11 as Rec11
import Rec12 as Rec12
import Rec13 as Rec13
import Rec14 as Rec14
import Rec15 as Rec15
import Rec2 as Rec2
import Rec3 as Rec3
import Rec4 as Rec4
import Rec5 as Rec5
import Rec6 as Rec6
import Rec7 as Rec7
import Rec8 as Rec8
import Rec9 as Rec9

-- Fully left-associated tree is worst-case stack behavior.
genBinL :: forall f a. (f -> a -> f -> f) -> f -> a -> Int -> f
genBinL branch tip val = go tip
  where
  go acc n
    | n <= 0 = acc
    | otherwise = go (branch acc val tip) (n - 1)

main :: Effect Unit
main = do
  let _ = Rec1.mapBin (add 1) (genBinL Rec1.Branch Rec1.Tip 0 1000)
  let _ = Rec2.mapBin (add 1) (genBinL Rec2.Branch Rec2.Tip 0 1000)
  let _ = Rec3.mapBin (add 1) (genBinL Rec3.Branch Rec3.Tip 0 1000)
  let _ = Rec4.mapBin (add 1) (genBinL Rec4.Branch Rec4.Tip 0 1000)
  let _ = Rec5.mapBin (add 1) (genBinL Rec5.Branch Rec5.Tip 0 1000)
  let _ = Rec6.mapBin (add 1) (genBinL Rec6.Branch Rec6.Tip 0 100000)
  let _ = Rec7.mapBin (add 1) (genBinL Rec7.Branch Rec7.Tip 0 100000)
  let _ = Rec8.mapBin (add 1) (genBinL Rec8.Branch Rec8.Tip 0 100000)
  let _ = Rec9.mapBin (add 1) (genBinL Rec9.Branch Rec9.Tip 0 100000)
  let _ = Rec10.mapBin (add 1) (genBinL Rec10.Branch Rec10.Tip 0 100000)
  let _ = Rec11.mapBin (add 1) (genBinL Rec11.Branch Rec11.Tip 0 100000)
  let _ = Rec12.mapBin (add 1) (genBinL Rec12.Branch Rec12.Tip 0 100000)
  let _ = Rec13.mapBin (add 1) (genBinL Rec13.Branch Rec13.Tip 0 100000)
  let _ = Rec14.mapBin (add 1) (genBinL Rec14.Branch Rec14.Tip 0 100000)
  let _ = Rec15.mapBin (add 1) (genBinL Rec15.Branch Rec15.Tip 0 100000)
  log "Done"
