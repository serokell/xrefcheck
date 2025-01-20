{- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.MaxHeaderLengthSpec where

import Universum hiding ((.~))

import Control.Lens ((.~))
import Data.Set qualified as S
import Network.HTTP.Client (newManager, managerSetMaxHeaderLength, defaultManagerSettings)
import Network.HTTP.Types (ok200)
import Network.Wai qualified as Web
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Web.Scotty qualified as Web
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Test.Xrefcheck.UtilRequests
import Xrefcheck.Config
import Xrefcheck.Progress
import Xrefcheck.Verify

mockHeader :: Int -> IO Web.Application
mockHeader size = Web.scottyApp $ do
  Web.matchAny "/header" $ do
    Web.setHeader "X-header" (TL.fromStrict $ T.replicate size "x")
    Web.status ok200

test_maxHeaderLength :: TestTree
test_maxHeaderLength = testGroup "MaxHeaderLength tests"
  [ testCase "Succeeds with small header" $ do
      setRef <- newIORef S.empty
      mgr <- newManager $ managerSetMaxHeaderLength mhl defaultManagerSettings
      checkMultipleLinksWithServer
        (5001, mockHeader (mhl `div` 2))
        setRef
        [ VerifyLinkTestEntry
            { vlteConfigModifier = \c -> c
                & cNetworkingL . ncMaxHeaderLengthL .~ mhl
                & cNetworkingL . ncHttpManagerL .~ Just mgr
            , vlteLink = "http://127.0.0.1:5001/header"
            , vlteExpectedProgress = mkProgressWithOneTask True
            , vlteExpectationErrors = VerifyResult []
            }
        ]

  , testCase "Fails with MaxHeaderLengthError" $ do
      setRef <- newIORef S.empty
      mgr <- newManager $ managerSetMaxHeaderLength mhl defaultManagerSettings
      checkMultipleLinksWithServer
        (5002, mockHeader (mhl*2))
        setRef
        [ VerifyLinkTestEntry
            { vlteConfigModifier = \c -> c
                & cNetworkingL . ncMaxHeaderLengthL .~ mhl
                & cNetworkingL . ncHttpManagerL .~ Just mgr
            , vlteLink = "http://127.0.0.1:5002/header"
            , vlteExpectedProgress = mkProgressWithOneTask False
            , vlteExpectationErrors = VerifyResult [MaxHeaderLengthError mhl]
            }
        ]
  ]
  where
    mhl = 4096

    mkProgressWithOneTask shouldSucceed = report "" $ initProgress 1
      where
        report =
          if shouldSucceed
          then reportSuccess
          else reportError
