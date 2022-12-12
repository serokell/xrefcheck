{- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.RedirectRequestsSpec where

import Universum

import Data.CaseInsensitive qualified as CI
import Data.Map qualified as M
import Network.HTTP.Types (Status, mkStatus)
import Network.HTTP.Types.Header (hLocation)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)
import Web.Firefly (ToResponse (toResponse), route, run)

import Test.Xrefcheck.UtilRequests
import Xrefcheck.Progress
import Xrefcheck.Verify

test_redirectRequests :: TestTree
test_redirectRequests = testGroup "Redirect response tests"
  [ testGroup "Temporary" $ temporaryRedirectTests <$> [302, 303, 307]
  , testGroup "Permanent" $ permanentRedirectTests <$> [301, 308]
  ]
  where
    url :: Text
    url = "http://127.0.0.1:5000/redirect"

    location :: Maybe Text
    location = Just "http://127.0.0.1:5000/other"

    temporaryRedirectTests :: Int -> TestTree
    temporaryRedirectTests statusCode =
      redirectTests
        (show statusCode <> " passes by default")
        (mkStatus statusCode "Temporary redirect")
        (const Nothing)

    permanentRedirectTests :: Int -> TestTree
    permanentRedirectTests statusCode =
      redirectTests
        (show statusCode <> " fails by default")
        (mkStatus statusCode "Permanent redirect")
        (Just . PermanentRedirectError url)

    redirectTests :: TestName -> Status -> (Maybe Text -> Maybe VerifyError) -> TestTree
    redirectTests name expectedStatus expectedError =
      testGroup name
        [
          testCase "With no location" $
            redirectAssertion expectedStatus Nothing (expectedError Nothing),
          testCase "With location" $
            redirectAssertion expectedStatus location (expectedError location)
        ]

    redirectAssertion :: Status -> Maybe Text -> Maybe VerifyError -> Assertion
    redirectAssertion expectedStatus expectedLocation expectedError =
      checkLinkAndProgressWithServer
        (mockRedirect expectedLocation expectedStatus)
        url
        (Progress
          { pTotal = 1
          , pCurrent = 1
          , pErrorsUnfixable = length $ maybeToList expectedError
          , pErrorsFixable = 0
          , pTaskTimestamp = Nothing
          }
        )
        (VerifyResult $ maybeToList expectedError)

    mockRedirect :: Maybe Text -> Status -> IO ()
    mockRedirect expectedLocation expectedSocation =
      run 5000 $ route "/redirect" $ pure $ toResponse
        ( "" :: Text
        , expectedSocation
        , M.fromList [(CI.map (decodeUtf8 @Text) hLocation, maybeToList expectedLocation)]
        )
