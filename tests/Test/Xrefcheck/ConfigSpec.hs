{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.ConfigSpec where

import qualified Data.ByteString as BS
import Test.Hspec (Spec, before, describe, it)
import Test.QuickCheck (counterexample, ioProperty, once)

import Xrefcheck.Config
import Xrefcheck.Core

spec :: Spec
spec = do
  describe "Default config is valid" $
    forM_ allFlavors $ \flavor ->
      it (show flavor) $
        once . ioProperty $ evaluateWHNF_ @_ @Config (defConfig flavor)

  describe "Filled default config matches the expected format" $
    before (BS.readFile "tests/configs/github-config.yaml") $
      -- The config we match against can be regenerated with
      -- stack exec xrefcheck -- dump-config -t GitHub -o tests/configs/github-config.yaml
      it "Config matches" $
        \config ->
          counterexample
            (toString $ unwords
             [ "Config does not match the expected format."
             , "Run"
             , "`stack exec xrefcheck -- dump-config -t GitHub -o tests/configs/github-config.yaml`"
             , "and verify changes"
             ]
            )
            (config == defConfigText GitHub)
