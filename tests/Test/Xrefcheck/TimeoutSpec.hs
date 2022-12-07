{- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.TimeoutSpec where

import Universum

import Data.CaseInsensitive qualified as CI
import Data.Map qualified as M
import Data.Set qualified as S
import Network.HTTP.Types (ok200, tooManyRequests429)
import Network.HTTP.Types.Header (hRetryAfter)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Time (Second, Time, sec, threadDelay)
import Web.Firefly (ToResponse (toResponse), route, run)

import Test.Xrefcheck.UtilRequests
import Xrefcheck.Config
import Xrefcheck.Progress
import Xrefcheck.Verify

-- Here all the delays are doubled because we call sites
-- with HEAD first and then GET methods.
test_timeout :: TestTree
test_timeout = testGroup "Timeout tests"
  [ testCase "Succeeds on one timeout if there were no 429 responses and no retries allowed" $
      timeoutTestCase [Delay] True
        (cNetworkingL . ncMaxTimeoutRetriesL .~ 0)
  , testCase "Returns an error on two timeouts if there were no 429 responses" $
      timeoutTestCase [Delay, Delay] False
        (cNetworkingL . ncMaxTimeoutRetriesL .~ 2)
  , testCase "Returns an error if there were 429 but no timeouts allowed" $
      timeoutTestCase [Respond429, Delay, Delay] False
        (cNetworkingL . ncMaxTimeoutRetriesL .~ 0)
  , testCase "Succeeds if there were 429 and one timeout allowed" $
      timeoutTestCase [Respond429, Delay, Delay] True
        (cNetworkingL . ncMaxTimeoutRetriesL .~ 1)
  , testCase "Fails on second timeout if there were 429 and one timeout allowed" $
      timeoutTestCase [Respond429, Delay, Delay, Delay, Delay] False
        (cNetworkingL . ncMaxTimeoutRetriesL .~ 1)
  , testCase "Fails on maximum allowed errors achieved (mixed errors)" $
      timeoutTestCase [Respond429, Delay, Delay, Respond429, Delay, Delay] False $ \c -> c
        & cNetworkingL . ncMaxTimeoutRetriesL .~ 3
        & cNetworkingL . ncMaxRetriesL .~ 3
  , testCase "Fails on timeout if another domain returned 429" $ do
      setRef <- newIORef S.empty
      checkMultipleLinksWithServer
        (mockTimeout (sec 0.4) [Respond429, Ok, Delay, Delay])
        setRef
        [ VerifyLinkTestEntry
            { vlteConfigModifier = \c -> c
                & cNetworkingL . ncMaxRetriesL .~ 1
                & setAllowedTimeout
            , vlteLink = "http://127.0.0.1:5000/timeout"
            , vlteExpectedProgress = mkProgressWithOneTask True
            , vlteExpectationErrors = VerifyResult []
            }
        , VerifyLinkTestEntry
            { vlteConfigModifier = \c -> c
                & cNetworkingL . ncMaxTimeoutRetriesL .~ 0
                & setAllowedTimeout
            , vlteLink = "http://localhost:5000/timeout"
            , vlteExpectedProgress = mkProgressWithOneTask False
            , vlteExpectationErrors = VerifyResult
              [ ExternalHttpTimeout (Just $ DomainName "localhost")
              ]
            }
        ]
  , testCase "Succeeds on timeout if another path of this domain returned 429" $ do
      setRef <- newIORef S.empty
      checkMultipleLinksWithServer
        (mockTimeout (sec 0.4) [Respond429, Ok, Delay, Delay])
        setRef
        [ VerifyLinkTestEntry
            { vlteConfigModifier = \c -> c
                & cNetworkingL . ncMaxRetriesL .~ 1
                & setAllowedTimeout
            , vlteLink = "http://127.0.0.1:5000/timeout"
            , vlteExpectedProgress = mkProgressWithOneTask True
            , vlteExpectationErrors = VerifyResult []
            }
        , VerifyLinkTestEntry
            { vlteConfigModifier = \c -> c
                & cNetworkingL . ncMaxTimeoutRetriesL .~ 1
                & setAllowedTimeout
            , vlteLink = "http://127.0.0.1:5000/timeoutother"
            , vlteExpectedProgress = mkProgressWithOneTask True
            , vlteExpectationErrors = VerifyResult []
            }
        ]
  ]
  where
    setAllowedTimeout = cNetworkingL . ncExternalRefCheckTimeoutL .~ (sec 0.25)

    mkProgressWithOneTask shouldSucceed =
      Progress
        { pTotal = 1
        , pCurrent = 1
        , pErrorsUnfixable = if shouldSucceed then 0 else 1
        , pErrorsFixable = 0
        , pTaskTimestamp = Nothing
        }

    timeoutTestCase mockResponses shouldSucceed configModifier = do
      let prog = mkProgressWithOneTask shouldSucceed
      setRef <- newIORef S.empty
      checkLinkAndProgressWithServer
        (\c -> c
          & setAllowedTimeout
          & configModifier)
        setRef
        (mockTimeout (sec 0.4) mockResponses)
        "http://127.0.0.1:5000/timeout" prog $
          VerifyResult $
            [ExternalHttpTimeout $ Just (DomainName "127.0.0.1") | not shouldSucceed]

    -- When called for the first (N-1) times, waits for specified
    -- amount of seconds and returns an arbitrary result.
    -- When called N time returns the result immediately.
    mockTimeout :: Time Second -> [MockTimeoutBehaviour] -> IO ()
    mockTimeout timeout behList = do
      ref <- newIORef @_ behList
      run 5000 $ do
        route "/timeout" $ handler ref
        route "/timeoutother" $ handler ref
      where
        handler ref = do
          mbCurrentAction <- atomicModifyIORef' ref $ \case
            b : bs -> (bs, Just b)
            [] -> ([], Nothing)
          let success = toResponse ("" :: Text, ok200, M.empty @(CI.CI Text) @[Text])
          case mbCurrentAction of
            Nothing -> pure success
            Just Ok -> pure success
            Just Delay -> do
              threadDelay timeout
              pure $ toResponse ("" :: Text, ok200, M.empty @(CI.CI Text) @[Text])
            Just Respond429 ->
              pure $ toResponse
                ("" :: Text, tooManyRequests429,
                  M.fromList [(CI.map (decodeUtf8 @Text) hRetryAfter, ["1" :: Text])])

data MockTimeoutBehaviour = Respond429 | Delay | Ok
