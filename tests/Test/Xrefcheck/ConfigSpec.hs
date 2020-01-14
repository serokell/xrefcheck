{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.ConfigSpec where

import Test.Hspec (Spec, it)
import Test.QuickCheck (ioProperty, once)

import Xrefcheck.Config

spec :: Spec
spec =
  it "Default config is valid" $
    once . ioProperty $ evaluateWHNF_ @_ @Config defConfig
