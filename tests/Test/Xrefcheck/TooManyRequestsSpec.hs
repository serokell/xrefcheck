{- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.TooManyRequestsSpec where

import Universum

import Data.CaseInsensitive qualified as CI
import Data.Set qualified as S
import Data.Time (addUTCTime, defaultTimeLocale, formatTime, getCurrentTime, rfc822DateFormat)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Types (Status (..), ok200, serviceUnavailable503, tooManyRequests429)
import Network.HTTP.Types.Header (HeaderName, hRetryAfter)
import Network.Wai (requestMethod)
import Network.Wai qualified as Web
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Time (sec, (-:-))
import Web.Scotty qualified as Web

import Test.Xrefcheck.UtilRequests
import Xrefcheck.Core
import Xrefcheck.Progress
import Xrefcheck.Util
import Xrefcheck.Verify

test_tooManyRequests :: TestTree
test_tooManyRequests = testGroup "429 response tests"
  [ testCase "Returns 200 eventually" $ do
      setRef <- newIORef S.empty
      let prog = reportSuccess "" $ initProgress 1
      checkLinkAndProgressWithServerDefault setRef (5000, mock429 "1" ok200)
        "http://127.0.0.1:5000/429" prog $ VerifyResult []
  , testCase "Returns 503 eventually" $ do
      setRef <- newIORef S.empty
      let prog = reportError "" $ initProgress 1
      checkLinkAndProgressWithServerDefault setRef (5000, mock429 "1" serviceUnavailable503)
        "http://127.0.0.1:5000/429" prog $ VerifyResult
          [ ExternalHttpResourceUnavailable $
              Status { statusCode = 503, statusMessage = "Service Unavailable"}
          ]
  , testCase "Successfully updates the new retry-after value (as seconds)" $ do
      withServer (5000, mock429 "2" ok200) $ do
        now <- getPOSIXTime <&> posixTimeToTimeSecond
        setRef <- newIORef S.empty
        progressRef <- newIORef VerifyProgress
              { vrLocal = initProgress 0
              , vrExternal = setTaskTimestamp "" (sec 3) (now -:- sec 1.5)
                  . reportSuccess ""
                  $ initProgress 2
              }
        _ <- verifyReferenceWithProgressDefault
          (Reference "" (Position Nothing) $ RIExternal $ ELUrl "http://127.0.0.1:5000/429")
          setRef
          progressRef
        progress <- vrExternal <$> readIORef progressRef
        let ttc = ttTimeToCompletion <$> getTaskTimestamp progress
        flip assertBool (ttc == Just (sec 2)) $
          "Expected time to completion be equal to " ++ show (Just $ sec 2) ++
          ", but instead it's " ++ show ttc
  , testCase "Successfully updates the new retry-after value (as date)" $ do
      utctime <- getCurrentTime
      let
        -- Set the @Retry-After@ response header value as (current datetime + 4 seconds)
        retryAfter = formatTime defaultTimeLocale rfc822DateFormat (addUTCTime 4 utctime)
        now = utcTimeToTimeSecond utctime
      withServer (5000, mock429 (fromString retryAfter) ok200) $ do
        setRef <- newIORef S.empty
        progressRef <- newIORef VerifyProgress
              { vrLocal = initProgress 0
              , vrExternal = setTaskTimestamp "" (sec 2) (now -:- sec 1.5)
                  . reportSuccess ""
                  $ initProgress 2
              }
        _ <- verifyReferenceWithProgressDefault
          (Reference "" (Position Nothing) $ RIExternal $ ELUrl "http://127.0.0.1:5000/429")
          setRef
          progressRef
        progress <- vrExternal <$> readIORef progressRef
        let ttc = fromMaybe (sec 0) $ ttTimeToCompletion <$> getTaskTimestamp progress
        flip assertBool (sec 3 <= ttc && ttc <= sec 4) $
          "Expected time to completion be within range (seconds): 3 <= x <= 4" ++
          ", but instead it's " ++ show ttc
  , testCase "Sets the new retry-after to 0 seconds if\
                    \ its value is a date && has already passed" $ do
      utctime <- getCurrentTime
      let
        -- Set the @Retry-After@ response header value as (current datetime - 4 seconds)
        retryAfter = formatTime defaultTimeLocale rfc822DateFormat (addUTCTime (-4) utctime)
        now = utcTimeToTimeSecond utctime
      withServer (5000, mock429 (fromString retryAfter) ok200) $ do
        setRef <- newIORef S.empty
        progressRef <- newIORef VerifyProgress
              { vrLocal = initProgress 0
              , vrExternal = setTaskTimestamp "" (sec 1) (now -:- sec 1.5)
                  . reportSuccess ""
                  $ initProgress 2
              }
        _ <- verifyReferenceWithProgressDefault
          (Reference "" (Position Nothing) $ RIExternal $ ELUrl "http://127.0.0.1:5000/429")
          setRef
          progressRef
        progress <- vrExternal <$> readIORef progressRef
        let ttc = ttTimeToCompletion <$> getTaskTimestamp progress
        flip assertBool (ttc == Just (sec 0)) $
          "Expected time to completion be 0 seconds" ++
          ", but instead it's " ++ show ttc
    , testCase "The GET request should not be attempted after catching a 429" $ do
      let
        mock429WithGlobalIORef :: IORef [(Text, Status)] -> IO Web.Application
        mock429WithGlobalIORef infoReverseAccumulatorRef = do
          callCountRef <- newIORef @_ @Int 0
          Web.scottyApp $
            Web.matchAny "/429grandfinale" $ do
              req <- Web.request
              let m = decodeUtf8 (requestMethod req)
              callCount <- atomicModifyIORef' callCountRef $ \cc -> (cc + 1, cc)
              atomicModifyIORef' infoReverseAccumulatorRef $ \lst ->
                ( ( m
                  , if | m == "GET"     -> ok200
                       | callCount == 0 -> tooManyRequests429
                       | otherwise      -> serviceUnavailable503
                  ) : lst
                , ()
                )
              if
                | m == "GET"     -> Web.status ok200
                | callCount == 0 -> do
                    Web.status tooManyRequests429
                    setHeader hRetryAfter "1"
                | otherwise      -> Web.status serviceUnavailable503
      infoReverseAccumulatorRef <- newIORef []
      setRef <- newIORef S.empty
      withServer (5000, mock429WithGlobalIORef infoReverseAccumulatorRef) $ do
        _ <- verifyLinkDefault setRef "http://127.0.0.1:5000/429grandfinale"
        infoReverseAccumulator <- readIORef infoReverseAccumulatorRef
        reverse infoReverseAccumulator @?=
          [ ("HEAD", tooManyRequests429)
          , ("HEAD", serviceUnavailable503)
          , ("GET", ok200)
          ]
  ]
  where
    -- When called for the first time, returns with a 429 and `Retry-After: @retryAfter@`.
    -- Subsequent calls will respond with @status@.
    mock429 :: Text -> Status -> IO Web.Application
    mock429 retryAfter status = do
      callCountRef <- newIORef @_ @Int 0
      Web.scottyApp $
        Web.matchAny "/429" $ do
          callCount <- atomicModifyIORef' callCountRef $ \cc -> (cc + 1, cc)
          if callCount == 0
          then do
            setHeader hRetryAfter retryAfter
            Web.status tooManyRequests429
          else do
            Web.status status

    setHeader :: HeaderName -> Text -> Web.ActionM ()
    setHeader hdr value = Web.setHeader (decodeUtf8 (CI.original hdr)) (fromStrict value)
