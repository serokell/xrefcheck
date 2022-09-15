{- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.TooManyRequestsSpec where

import Universum

import Control.Concurrent (forkIO, killThread)
import Control.Exception qualified as E
import Data.CaseInsensitive qualified as CI
import Data.Map qualified as M
import Data.Time (addUTCTime, formatTime, getCurrentTime, defaultTimeLocale, rfc822DateFormat)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Fmt (indentF, pretty, unlinesF)
import Network.HTTP.Types (Status (..), ok200, serviceUnavailable503, tooManyRequests429)
import Network.HTTP.Types.Header (hRetryAfter)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.HUnit (assertBool)
import Time (sec, (-:-))
import Web.Firefly (ToResponse (toResponse), route, run, getMethod)

import Xrefcheck.Config
import Xrefcheck.Core
import Xrefcheck.Progress
import Xrefcheck.Util
import Xrefcheck.Verify

spec :: Spec
spec = do
  describe "429 response tests" $ do
    it "Returns 200 eventually" $ do
      let prog = Progress{ pTotal = 1
                         , pCurrent = 1
                         , pErrorsUnfixable = 0
                         , pErrorsFixable = 0
                         , pTaskTimestamp = Nothing
                         }
      checkLinkAndProgressWithServer (mock429 "1" ok200)
        "http://127.0.0.1:5000/429" prog $ VerifyResult []
    it "Returns 503 eventually" $ do
      let prog = Progress{ pTotal = 1
                         , pCurrent = 1
                         , pErrorsUnfixable = 1
                         , pErrorsFixable = 0
                         , pTaskTimestamp = Nothing
                         }
      checkLinkAndProgressWithServer (mock429 "1" serviceUnavailable503)
        "http://127.0.0.1:5000/429" prog $ VerifyResult
          [ ExternalHttpResourceUnavailable $
              Status { statusCode = 503, statusMessage = "Service Unavailable"}
          ]
    it "Successfully updates the new retry-after value (as seconds)" $ do
      E.bracket (forkIO $ mock429 "2" ok200) killThread $ \_ -> do
        now <- getPOSIXTime <&> posixTimeToTimeSecond
        progressRef <- newIORef VerifyProgress
              { vrLocal = initProgress 0
              , vrExternal = Progress
                  { pTotal = 2
                  , pCurrent = 1
                  , pErrorsUnfixable = 0
                  , pErrorsFixable = 0
                  , pTaskTimestamp = Just (TaskTimestamp (sec 3) (now -:- sec 1.5))
                  }
              }
        _ <- verifyReferenceWithProgress
          (Reference "" "http://127.0.0.1:5000/429" Nothing (Position Nothing))
          progressRef
        Progress{..} <- vrExternal <$> readIORef progressRef
        let ttc = ttTimeToCompletion <$> pTaskTimestamp
        flip assertBool (ttc == Just (sec 2)) $
          "Expected time to completion be equal to " ++ show (Just $ sec 2) ++
          ", but instead it's " ++ show ttc
    it "Successfully updates the new retry-after value (as date)" $ do
      utctime <- getCurrentTime
      let
        -- Set the @Retry-After@ response header value as (current datetime + 4 seconds)
        retryAfter = formatTime defaultTimeLocale rfc822DateFormat (addUTCTime 4 utctime)
        now = utcTimeToTimeSecond utctime
      E.bracket (forkIO $ mock429 (fromString retryAfter) ok200) killThread $ \_ -> do
        progressRef <- newIORef VerifyProgress
              { vrLocal = initProgress 0
              , vrExternal = Progress
                  { pTotal = 2
                  , pCurrent = 1
                  , pErrorsUnfixable = 0
                  , pErrorsFixable = 0
                  , pTaskTimestamp = Just (TaskTimestamp (sec 2) (now -:- sec 1.5))
                  }
              }
        _ <- verifyReferenceWithProgress
          (Reference "" "http://127.0.0.1:5000/429" Nothing (Position Nothing))
          progressRef
        Progress{..} <- vrExternal <$> readIORef progressRef
        let ttc = fromMaybe (sec 0) $ ttTimeToCompletion <$> pTaskTimestamp
        flip assertBool (sec 3 <= ttc && ttc <= sec 4) $
          "Expected time to completion be within range (seconds): 3 <= x <= 4" ++
          ", but instead it's " ++ show ttc
    it "Sets the new retry-after to 0 seconds if its value is a date && has already passed" $ do
      utctime <- getCurrentTime
      let
        -- Set the @Retry-After@ response header value as (current datetime - 4 seconds)
        retryAfter = formatTime defaultTimeLocale rfc822DateFormat (addUTCTime (-4) utctime)
        now = utcTimeToTimeSecond utctime
      E.bracket (forkIO $ mock429 (fromString retryAfter) ok200) killThread $ \_ -> do
        progressRef <- newIORef VerifyProgress
              { vrLocal = initProgress 0
              , vrExternal = Progress
                  { pTotal = 2
                  , pCurrent = 1
                  , pErrorsUnfixable = 0
                  , pErrorsFixable = 0
                  , pTaskTimestamp = Just (TaskTimestamp (sec 1) (now -:- sec 1.5))
                  }
              }
        _ <- verifyReferenceWithProgress
          (Reference "" "http://127.0.0.1:5000/429" Nothing (Position Nothing))
          progressRef
        Progress{..} <- vrExternal <$> readIORef progressRef
        let ttc = ttTimeToCompletion <$> pTaskTimestamp
        flip assertBool (ttc == Just (sec 0)) $
          "Expected time to completion be 0 seconds" ++
          ", but instead it's " ++ show ttc
    it "The GET request should not be attempted after catching a 429" $ do
      let
        mock429WithGlobalIORef :: IORef [(Text, Status)] -> IO ()
        mock429WithGlobalIORef infoReverseAccumulatorRef = do
          callCountRef <- newIORef @_ @Int 0
          run 5000 $ do
            route "/429grandfinale" $ do
              m <- getMethod
              callCount <- atomicModifyIORef' callCountRef $ \cc -> (cc + 1, cc)
              atomicModifyIORef' infoReverseAccumulatorRef $ \lst ->
                ( ( m
                  , if | m == "GET" -> ok200
                       | callCount == 0 -> tooManyRequests429
                       | otherwise -> serviceUnavailable503
                  ) : lst
                , ()
                )
              pure $ if
                | m == "GET" -> toResponse ("" :: Text, ok200)
                | callCount == 0 -> toResponse
                    ( "" :: Text
                    , tooManyRequests429
                    , M.fromList [(CI.map (decodeUtf8 @Text) hRetryAfter, ["1" :: Text])]
                    )
                | otherwise -> toResponse ("" :: Text, serviceUnavailable503)
      infoReverseAccumulatorRef <- newIORef []
      E.bracket (forkIO $ mock429WithGlobalIORef infoReverseAccumulatorRef) killThread $ \_ -> do
        _ <- verifyLink "http://127.0.0.1:5000/429grandfinale"
        infoReverseAccumulator <- readIORef infoReverseAccumulatorRef
        reverse infoReverseAccumulator `shouldBe`
          [ ("HEAD", tooManyRequests429)
          , ("HEAD", serviceUnavailable503)
          , ("GET", ok200)
          ]
  where
    checkLinkAndProgressWithServer mock link progress vrExpectation =
      E.bracket (forkIO mock) killThread $ \_ -> do
        (result, progRes) <- verifyLink link
        flip assertBool (result == vrExpectation) . pretty $ unlinesF
          [ "Verification results differ: expected"
          , indentF 2 (show vrExpectation)
          , "but got"
          , indentF 2 (show result)
          ]
        flip assertBool (progRes `progEquiv` progress) . pretty $ unlinesF
          [ "Expected the progress bar state to be"
          , indentF 2 (show progress)
          , "but got"
          , indentF 2 (show progRes)
          ]
      where
        -- | Check whether the two @Progress@ values are equal up to similarity of their essential
        -- components, ignoring the comparison of @pTaskTimestamp@s, which is done to prevent test
        -- failures when comparing the resulting progress, gotten from running the link
        -- verification algorithm, with the expected one, where @pTaskTimestamp@ is hardcoded
        -- as @Nothing@.
        progEquiv :: Eq a => Progress a -> Progress a -> Bool
        progEquiv p1 p2 = and [ ((==) `on` pCurrent) p1 p2
                              , ((==) `on` pTotal) p1 p2
                              , ((==) `on` pErrorsUnfixable) p1 p2
                              , ((==) `on` pErrorsFixable) p1 p2
                              ]

    verifyLink :: Text -> IO (VerifyResult VerifyError, Progress Int)
    verifyLink link = do
      let reference = Reference "" link Nothing (Position Nothing)
      progRef <- newIORef $ initVerifyProgress [reference]
      result <- verifyReferenceWithProgress reference progRef
      progress <- readIORef progRef
      return (result, vrExternal progress)

    verifyReferenceWithProgress :: Reference -> IORef VerifyProgress -> IO (VerifyResult VerifyError)
    verifyReferenceWithProgress reference progRef = do
      fmap wrlItem <$> verifyReference
        ((cVerification $ defConfig GitHub) { vcIgnoreRefs = [] }) FullMode
        progRef (RepoInfo M.empty) "." "" reference

    -- | When called for the first time, returns with a 429 and `Retry-After: @retryAfter@`.
    -- Subsequent calls will respond with @status@.
    mock429 :: Text -> Status -> IO ()
    mock429 retryAfter status = do
      callCountRef <- newIORef @_ @Int 0
      run 5000 $
        route "/429" $ do
          callCount <- atomicModifyIORef' callCountRef $ \cc -> (cc + 1, cc)
          pure $
            if callCount == 0
            then toResponse
              ( "" :: Text
              , tooManyRequests429
              , M.fromList [(CI.map (decodeUtf8 @Text) hRetryAfter, [retryAfter])]
              )
            else toResponse ("" :: Text, status)
