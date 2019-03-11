module Test.Crv.AnchorsSpec where

import Test.Hspec (Spec, it)
import Test.QuickCheck (conjoin, (===))

import Crv.Core (headerToAnchor)

spec :: Spec
spec = do
    it "Header-to-anchor conversion" $
        conjoin $ map (\(a, b) -> headerToAnchor a === b) $
        [

        ]
