{- SPDX-FileCopyrightText: 2019-2021 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.ConfigSpec where

import Universum

import Control.Concurrent (forkIO, killThread)
import Control.Exception qualified as E

import Data.ByteString qualified as BS
import Network.HTTP.Types (Status (..))
import Test.Hspec (Spec, before, describe, it, shouldBe)
import Test.QuickCheck (counterexample, ioProperty, once)

import Xrefcheck.Config (Config (..), VerifyConfig (..), defConfig, defConfigText)
import Xrefcheck.Core (Flavor (GitHub), allFlavors)
import Xrefcheck.Verify (VerifyError (..), VerifyResult (..), checkExternalResource)

import Test.Xrefcheck.Util (mockServer)

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
          let matches =
                [ "Config does not match the expected format."
                , "Run"
                , "`stack exec xrefcheck -- dump-config -t GitHub -o tests/configs/github-config.yaml`"
                , "and verify changes"
                ]
          in
            counterexample
            (toString $ unwords matches)
            (config == defConfigText GitHub)

  describe "`ignoreAuthFailures` working as expected" $ do
    let config = (cVerification $ defConfig GitHub) { vcCheckLocalhost = True }

    it "when True - assume 401 status is valid" $
      checkLinkWithServer (config { vcIgnoreAuthFailures = True })
        "http://127.0.0.1:3000/401" $ VerifyResult []

    it "when False - assume 401 status is invalid" $
      checkLinkWithServer (config { vcIgnoreAuthFailures = False })
        "http://127.0.0.1:3000/401" $ VerifyResult
          [ ExternalHttpResourceUnavailable $
              Status { statusCode = 401, statusMessage = "Unauthorized" }
          ]

    it "when True - assume 403 status is valid" $
      checkLinkWithServer (config { vcIgnoreAuthFailures = True })
        "http://127.0.0.1:3000/403" $ VerifyResult []

    it "when False - assume 403 status is invalid" $
      checkLinkWithServer (config { vcIgnoreAuthFailures = False })
        "http://127.0.0.1:3000/403" $ VerifyResult
          [ ExternalHttpResourceUnavailable $
              Status { statusCode = 403, statusMessage = "Forbidden" }
          ]
  where
    checkLinkWithServer config link expectation =
      E.bracket (forkIO mockServer) killThread $ \_ -> do
        result <- checkExternalResource config link
        result `shouldBe` expectation
