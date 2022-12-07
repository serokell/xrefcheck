{- SPDX-FileCopyrightText: 2019-2021 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.ConfigSpec where

import Universum

import Control.Concurrent (forkIO, killThread)
import Control.Exception qualified as E

import Data.List (isInfixOf)
import Data.Yaml (ParseException (..), decodeEither')
import Network.HTTP.Types (Status (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (ioProperty, testProperty)

import Xrefcheck.Config
  (Config, cExclusionsL, cNetworkingL, defConfig, defConfigText, ncIgnoreAuthFailuresL)
import Xrefcheck.Core (Flavor (GitHub), allFlavors)
import Xrefcheck.Scan (ecIgnoreExternalRefsToL)
import Xrefcheck.Verify (VerifyError (..), checkExternalResource)

import Test.Xrefcheck.Util (mockServer)

test_config :: [TestTree]
test_config =
  [ testGroup "Default config is valid" [
      testProperty (show flavor) $
        ioProperty $ evaluateWHNF_ @_ @Config (defConfig flavor)
        | flavor <- allFlavors]

  , testGroup "Filled default config matches the expected format"
    -- The config we match against can be regenerated with
    -- stack exec xrefcheck -- dump-config -t GitHub -o tests/configs/github-config.yaml --force
      [ testCase "Config matches" $ do
        config <- readFile "tests/configs/github-config.yaml"
        when (config /= defConfigText GitHub) $
          assertFailure $ toString $ unwords
            [ "Config does not match the expected format."
            , "Run"
            , "`stack exec xrefcheck -- dump-config -t GitHub -o tests/configs/github-config.yaml --force`"
            , "and verify changes"
            ]
      ]
  , testGroup "`ignoreAuthFailures` working as expected" $
    let config = defConfig GitHub & cExclusionsL . ecIgnoreExternalRefsToL .~ []

        setIgnoreAuthFailures value =
          config & cNetworkingL . ncIgnoreAuthFailuresL .~ value
    in [ testCase "when True - assume 401 status is valid" $
          checkLinkWithServer (setIgnoreAuthFailures True)
            "http://127.0.0.1:3000/401" $ Right ()

       , testCase "when False - assume 401 status is invalid" $
          checkLinkWithServer (setIgnoreAuthFailures False)
            "http://127.0.0.1:3000/401" $
                Left $ ExternalHttpResourceUnavailable $
                  Status { statusCode = 401, statusMessage = "Unauthorized" }

       , testCase "when True - assume 403 status is valid" $
          checkLinkWithServer (setIgnoreAuthFailures True)
            "http://127.0.0.1:3000/403" $ Right ()

       , testCase "when False - assume 403 status is invalid" $
          checkLinkWithServer (setIgnoreAuthFailures False)
            "http://127.0.0.1:3000/403" $
                Left $ ExternalHttpResourceUnavailable $
                  Status { statusCode = 403, statusMessage = "Forbidden" }
       ]
  , testGroup "Config parser reject input with unknown fields"
      [ testCase "throws error with useful messages" $ do
          case decodeEither' @Config $ encodeUtf8 $ defConfigText GitHub <> "strangeField: []" of
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
        result <- runExceptT $ checkExternalResource config link
        result @?= expectation
