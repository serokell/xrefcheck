module Test.Crv.ConfigSpec where

import Test.Hspec (Spec, it)
import Test.QuickCheck (ioProperty, once)

import Crv.Config

spec :: Spec
spec =
  it "Default config is valid" $
    once . ioProperty $ evaluateWHNF_ @_ @Config defConfig
