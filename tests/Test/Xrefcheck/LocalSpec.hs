{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.LocalSpec where

import Universum

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck ((===))

import Xrefcheck.Core (canonizeLocalRef)

spec :: Spec
spec = do
  describe "Local refs canonizing" $ do
    it "Strips ./" $
      canonizeLocalRef "./AnchorsSpec.hs" === "AnchorsSpec.hs"

    it "Strips ././" $
      canonizeLocalRef "././AnchorsSpec.hs" === "AnchorsSpec.hs"

    it "Leaves plain other intact" $
      canonizeLocalRef "../AnchorsSpec.hs" === "../AnchorsSpec.hs"
