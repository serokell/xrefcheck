{- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.RedirectChainSpec where

import Universum hiding ((.~))

import Control.Lens ((.~))
import Data.CaseInsensitive qualified as CI
import Network.HTTP.Types (movedPermanently301)
import Network.HTTP.Types.Header (HeaderName, hLocation)
import Network.Wai qualified as Web
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Web.Scotty qualified as Web

import Test.Xrefcheck.UtilRequests
import Xrefcheck.Config
import Xrefcheck.Progress
import Xrefcheck.Verify

test_redirectRequests :: TestTree
test_redirectRequests = testGroup "Redirect chain tests"
  [ testCase "Missing location" $ do
      setRef <- newIORef mempty
      checkLinkAndProgressWithServer
        (configMod 5)
        setRef
        (5000, mockRedirect)
        (link "/broken1")
        progress
        (VerifyResult [RedirectMissingLocation $ chain [ "/broken1", "/broken2", "/broken3"]])
  , testCase "Cycle" $ do
      setRef <- newIORef mempty
      checkLinkAndProgressWithServer
        (configMod 5)
        setRef
        (5000, mockRedirect)
        (link "/cycle1")
        progress
        (VerifyResult [RedirectChainCycle $ chain ["/cycle1", "/cycle2", "/cycle3", "/cycle4", "/cycle2"]])
  , testGroup "Relative redirect"
      [ testCase "Host" $ do
          setRef <- newIORef mempty
          checkLinkAndProgressWithServer
            (configMod 1)
            setRef
            (5000, mockRedirect)
            (link "/relative/host")
            progress
            (VerifyResult [RedirectChainLimit $ chain ["/relative/host", "/cycle2", "/cycle3"]])
      , testCase "Path" $ do
          setRef <- newIORef mempty
          checkLinkAndProgressWithServer
            (configMod 1)
            setRef
            (5000, mockRedirect)
            (link "/relative/path")
            progress
            (VerifyResult [RedirectChainLimit $ chain ["/relative/path", "/relative/host", "/cycle2"]])
      ]
  , testCase "Other host redirect" $ withServer (5001, otherMockRedirect) $ do
      setRef <- newIORef mempty
      checkLinkAndProgressWithServer
        (configMod 1)
        setRef
        (5000, mockRedirect)
        "http://127.0.0.1:5001/other/host"
        progress
        (VerifyResult [RedirectChainLimit $ fromList ["http://127.0.0.1:5001/other/host", link "/relative/host", link "/cycle2"]])
  , testGroup "Limit"
      [ testCase "Takes effect" $ do
          setRef <- newIORef mempty
          checkLinkAndProgressWithServer
            (configMod 2)
            setRef
            (5000, mockRedirect)
            (link "/cycle1")
            progress
            (VerifyResult [RedirectChainLimit $ chain ["/cycle1", "/cycle2", "/cycle3", "/cycle4"]])
      , testCase "No redirects allowed" $ do
          setRef <- newIORef mempty
          checkLinkAndProgressWithServer
            (configMod 0)
            setRef
            (5000, mockRedirect)
            (link "/cycle1")
            progress
            (VerifyResult [RedirectChainLimit $ chain ["/cycle1", "/cycle2"]])
      , testCase "Negative" $ do
          setRef <- newIORef mempty
          checkLinkAndProgressWithServer
            (configMod (-1))
            setRef
            (5000, mockRedirect)
            (link "/cycle1")
            progress
            (VerifyResult [RedirectChainCycle $ chain ["/cycle1", "/cycle2", "/cycle3", "/cycle4", "/cycle2"]])
      ]
  ]
  where
    link :: Text -> Text
    link = ("http://127.0.0.1:5000" <>)

    chain :: [Text] -> RedirectChain
    chain = fromList . fmap link

    progress :: Progress Int Text
    progress = reportError "" $ initProgress 1

    configMod :: Int -> Config -> Config
    configMod limit config = config
      & cNetworkingL . ncExternalRefRedirectsL .~ [RedirectRule Nothing Nothing Nothing RROFollow]
      & cNetworkingL . ncMaxRedirectFollowsL .~ limit

    setHeader :: HeaderName -> Text -> Web.ActionM ()
    setHeader hdr value = Web.setHeader (decodeUtf8 (CI.original hdr)) (fromStrict value)

    mockRedirect :: IO Web.Application
    mockRedirect = do
      Web.scottyApp $ do
        -- A set of redirect routes that correspond to a broken chain.
        Web.matchAny "/broken1" $ do
          setHeader hLocation (link "/broken2")
          Web.status movedPermanently301
        Web.matchAny "/broken2" $ do
          setHeader hLocation (link "/broken3")
          Web.status movedPermanently301
        Web.matchAny "/broken3" $ do
          -- hLocation: no value
          Web.status movedPermanently301

        -- A set of redirect routes that correspond to a cycle.
        Web.matchAny "/cycle1" $ do
          setHeader hLocation (link "/cycle2")
          Web.status movedPermanently301
        Web.matchAny "/cycle2" $ do
          setHeader hLocation (link "/cycle3")
          Web.status movedPermanently301
        Web.matchAny "/cycle3" $ do
          setHeader hLocation (link "/cycle4")
          Web.status movedPermanently301
        Web.matchAny "/cycle4" $ do
          setHeader hLocation (link "/cycle2")
          Web.status movedPermanently301

        -- Relative redirects.
        Web.matchAny "/relative/host" $ do
          setHeader hLocation "/cycle2"
          Web.status movedPermanently301
        Web.matchAny "/relative/path" $ do
          setHeader hLocation "host"
          Web.status movedPermanently301

    -- To other host
    otherMockRedirect :: IO Web.Application
    otherMockRedirect =
      Web.scottyApp $ Web.matchAny "/other/host" $ do
        setHeader hLocation (link "/relative/host")
        Web.status movedPermanently301
