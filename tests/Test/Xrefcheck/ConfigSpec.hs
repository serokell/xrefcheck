{- SPDX-FileCopyrightText: 2019-2021 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.ConfigSpec where

import Universum

import Control.Concurrent (forkIO, killThread)
import Control.Exception qualified as E

import Data.ByteString qualified as BS
import Data.List (isInfixOf)
import Data.Yaml (ParseException (..), decodeEither')
import Network.HTTP.Types (Status (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (ioProperty, testProperty)


import Xrefcheck.Config (Config, Config' (..), VerifyConfig' (..), defConfig, defConfigText)
import Xrefcheck.Core (Flavor (GitHub), allFlavors)
import Xrefcheck.Verify (VerifyError (..), VerifyResult (..), checkExternalResource)

import Test.Xrefcheck.Util (mockServer)

test_config :: [TestTree]
test_config =
  [ testGroup "Default config is valid" [
      testProperty (show flavor) $
        ioProperty $ evaluateWHNF_ @_ @Config (defConfig flavor)
        | flavor <- allFlavors]

  , testGroup "Filled default config matches the expected format"
    -- The config we match against can be regenerated with
    -- stack exec xrefcheck -- dump-config -t GitHub -o tests/configs/github-config.yaml
      [ testCase "Config matches" $ do
        config <- BS.readFile "tests/configs/github-config.yaml"
        when (config /= defConfigText GitHub) $
          assertFailure $ toString $ unwords
            [ "Config does not match the expected format."
            , "Run"
            , "`stack exec xrefcheck -- dump-config -t GitHub -o tests/configs/github-config.yaml`"
            , "and verify changes"
            ]
      ]
  , testGroup "`ignoreAuthFailures` working as expected" $
    let config = (cVerification $ defConfig GitHub) { vcIgnoreRefs = [] }
    in [ testCase "when True - assume 401 status is valid" $
          checkLinkWithServer (config { vcIgnoreAuthFailures = True })
            "http://127.0.0.1:3000/401" $ VerifyResult []

       , testCase "when False - assume 401 status is invalid" $
          checkLinkWithServer (config { vcIgnoreAuthFailures = False })
            "http://127.0.0.1:3000/401" $ VerifyResult
              [ ExternalHttpResourceUnavailable $
                  Status { statusCode = 401, statusMessage = "Unauthorized" }
              ]

       , testCase "when True - assume 403 status is valid" $
          checkLinkWithServer (config { vcIgnoreAuthFailures = True })
            "http://127.0.0.1:3000/403" $ VerifyResult []

       , testCase "when False - assume 403 status is invalid" $
          checkLinkWithServer (config { vcIgnoreAuthFailures = False })
            "http://127.0.0.1:3000/403" $ VerifyResult
              [ ExternalHttpResourceUnavailable $
                  Status { statusCode = 403, statusMessage = "Forbidden" }
              ]
       ]
  , testGroup "Config parser reject input with unknown fields"
      [ testCase "throws error with useful messages" $ do
          case decodeEither' @Config (defConfigText GitHub <> "strangeField: []") of
            Left (AesonException str) ->
              if "unknown fields: [\"strangeField\"]" `isInfixOf` str
              then pure ()
              else assertFailure $ "Bad error message: " <> str
            _ -> assertFailure "Config parser accepted config with unknown field"
      ]
  ]




  where
    checkLinkWithServer config link expectation =
      E.bracket (forkIO mockServer) killThread $ \_ -> do
        result <- checkExternalResource config link
        result @?= expectation
