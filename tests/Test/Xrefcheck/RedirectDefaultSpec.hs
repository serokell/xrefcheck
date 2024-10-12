{- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.RedirectDefaultSpec where

import Universum

import Data.CaseInsensitive qualified as CI
import Data.Set qualified as S
import Network.HTTP.Types (Status, mkStatus)
import Network.HTTP.Types.Header (HeaderName, hLocation)
import Network.Wai.Handler.Warp qualified as Web
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)
import Web.Scotty qualified as Web

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
        ( (if isNothing expectedError then reportSuccess else reportError) "" $
            initProgress 1
        )
        (VerifyResult $ maybeToList expectedError)

    mockRedirect :: Maybe Text -> Status -> IO ()
    mockRedirect expectedLocation expectedStatus =
      Web.run 5000 <=< Web.scottyApp $
      Web.matchAny "/redirect" $ do
        whenJust expectedLocation (setHeader hLocation)
        Web.status expectedStatus

    setHeader :: HeaderName -> Text -> Web.ActionM ()
    setHeader hdr value = Web.setHeader (decodeUtf8 (CI.original hdr)) (fromStrict value)
