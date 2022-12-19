{- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.RedirectChainSpec where

import Universum

import Data.CaseInsensitive qualified as CI
import Data.Map qualified as M
import Network.HTTP.Types (mkStatus)
import Network.HTTP.Types.Header (hLocation)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Web.Firefly (App, ToResponse (toResponse), route, run)

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
        mockRedirect
        (link "/broken1")
        progress
        (VerifyResult [RedirectMissingLocation $ chain [ "/broken1", "/broken2", "/broken3"]])
  , testCase "Cycle" $ do
      setRef <- newIORef mempty
      checkLinkAndProgressWithServer
        (configMod 5)
        setRef
        mockRedirect
        (link "/cycle1")
        progress
        (VerifyResult [RedirectChainCycle $ chain ["/cycle1", "/cycle2", "/cycle3", "/cycle4", "/cycle2"]])
  , testGroup "Limit"
      [ testCase "Takes effect" $ do
          setRef <- newIORef mempty
          checkLinkAndProgressWithServer
            (configMod 2)
            setRef
            mockRedirect
            (link "/cycle1")
            progress
            (VerifyResult [RedirectChainLimit $ chain ["/cycle1", "/cycle2", "/cycle3", "/cycle4"]])
      , testCase "No redirects allowed" $ do
          setRef <- newIORef mempty
          checkLinkAndProgressWithServer
            (configMod 0)
            setRef
            mockRedirect
            (link "/cycle1")
            progress
            (VerifyResult [RedirectChainLimit $ chain ["/cycle1", "/cycle2"]])
      , testCase "Negative" $ do
          setRef <- newIORef mempty
          checkLinkAndProgressWithServer
            (configMod (-1))
            setRef
            mockRedirect
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

    progress :: Progress Int
    progress = Progress
      { pTotal = 1
      , pCurrent = 1
      , pErrorsUnfixable = 1
      , pErrorsFixable = 0
      , pTaskTimestamp = Nothing
      }

    configMod :: Int -> Config -> Config
    configMod limit config = config
      & cNetworkingL . ncExternalRefRedirectsL .~ [RedirectRule Nothing Nothing Nothing RROFollow]
      & cNetworkingL . ncMaxRedirectFollowsL .~ limit

    redirectRoute :: Text -> Maybe Text -> App ()
    redirectRoute name to = route name $ pure $ toResponse
      ( "" :: Text
      , mkStatus 301 "Permanent redirect"
      , M.fromList [(CI.map (decodeUtf8 @Text) hLocation, fmap link $ maybeToList to)]
      )

    mockRedirect :: IO ()
    mockRedirect =
      run 5000 do
        -- A set of redirect routes that correspond to a broken chain.
        redirectRoute "/broken1" $ Just "/broken2"
        redirectRoute "/broken2" $ Just "/broken3"
        redirectRoute "/broken3" Nothing

        -- A set of redirect routes that correspond to a cycle.
        redirectRoute "/cycle1" $ Just "/cycle2"
        redirectRoute "/cycle2" $ Just "/cycle3"
        redirectRoute "/cycle3" $ Just "/cycle4"
        redirectRoute "/cycle4" $ Just "/cycle2"
