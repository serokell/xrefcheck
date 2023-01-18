{- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.RedirectDefaultSpec where

import Universum

import Data.CaseInsensitive qualified as CI
import Data.Map qualified as M
import Data.Set qualified as S
import Network.HTTP.Types (Status, mkStatus)
import Network.HTTP.Types.Header (hLocation)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)
import Web.Firefly (ToResponse (toResponse), route, run)

import Test.Xrefcheck.UtilRequests
import Xrefcheck.Config
import Xrefcheck.Progress
import Xrefcheck.Verify

test_redirectRequests :: TestTree
test_redirectRequests = testGroup "Redirect response defaults"
  [ testGroup "Temporary" $ allowedRedirectTests <$> [302, 303, 307]
  , testGroup "Permanent" $ permanentRedirectTests <$> [301, 308]
  , testGroup "304 Not Modified" $ allowedRedirectTests <$> [304]
  ]
  where
    url :: Text
    url = "http://127.0.0.1:5000/redirect"

    location :: Maybe Text
    location = Just "http://127.0.0.1:5000/other"

    allowedRedirectTests :: Int -> TestTree
    allowedRedirectTests statusCode =
      redirectTests
        (show statusCode <> " passes by default")
        (mkStatus statusCode "Allowed redirect")
        (\case
          Nothing -> Just $ RedirectMissingLocation $ fromList [url]
          Just _ -> Nothing
        )

    permanentRedirectTests :: Int -> TestTree
    permanentRedirectTests statusCode =
      redirectTests
        (show statusCode <> " fails by default")
        (mkStatus statusCode "Permanent redirect")
        (\case
          Nothing -> Just $ RedirectMissingLocation $ fromList [url]
          Just loc -> Just $ RedirectRuleError (fromList [url, loc]) (Just RROPermanent)
        )

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
    redirectAssertion expectedStatus expectedLocation expectedError = do
      setRef <- newIORef S.empty
      checkLinkAndProgressWithServerDefault
        setRef
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
    mockRedirect expectedLocation expectedStatus =
      run 5000 $ route "/redirect" $ pure $ toResponse
        ( "" :: Text
        , expectedStatus
        , M.fromList [(CI.map (decodeUtf8 @Text) hLocation, maybeToList expectedLocation)]
        )
