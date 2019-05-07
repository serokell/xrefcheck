module Test.Crv.LocalSpec where

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck ((===))

import Crv.Core (canonizeLocalRef)

spec :: Spec
spec = do
    describe "Local refs canonizing" $ do
      it "Strips ./" $
        canonizeLocalRef "./AnchorsSpec.hs" === "AnchorsSpec.hs"

      it "Strips ././" $
        canonizeLocalRef "././AnchorsSpec.hs" === "AnchorsSpec.hs"

      it "Leaves plain other intact" $
        canonizeLocalRef "../AnchorsSpec.hs" === "../AnchorsSpec.hs"
