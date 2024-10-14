{- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.RedirectConfigSpec where

import Universum hiding ((%~), (.~))

import Control.Lens ((%~), (.~))
import Data.CaseInsensitive qualified as CI
import Network.HTTP.Types (found302, movedPermanently301, temporaryRedirect307)
import Network.HTTP.Types.Header (HeaderName, hLocation)
import Network.Wai.Handler.Warp qualified as Web
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Text.Regex.TDFA.Text qualified as R
import Web.Scotty qualified as Web

import Test.Xrefcheck.UtilRequests
import Xrefcheck.Config
import Xrefcheck.Progress
import Xrefcheck.Scan
import Xrefcheck.Verify

test_redirectRequests :: TestTree
test_redirectRequests = testGroup "Redirect config tests"
  [ testGroup "Match"
      [ testGroup "By \"on\""
          [ testCase "Do match" $ do
              setRef <- newIORef mempty
              checkLinkAndProgressWithServer
                (configMod [RedirectRule Nothing Nothing (Just RROTemporary) RROInvalid] [])
                setRef
                mockRedirect
                (link "/temporary-redirect")
                (progress False)
                (VerifyResult [RedirectRuleError (chain ["/temporary-redirect", "/ok"]) (Just RROTemporary)])
          , testCase "Do not match" $ do
              setRef <- newIORef mempty
              checkLinkAndProgressWithServer
                (configMod [RedirectRule Nothing Nothing (Just RROPermanent) RROInvalid] [])
                setRef
                mockRedirect
                (link "/temporary-redirect")
                (progress True)
                (VerifyResult [])
          ]
      , testGroup "By \"to\""
          [ testCase "Do match" $ do
              setRef <- newIORef mempty
              checkLinkAndProgressWithServer
                (configMod [RedirectRule Nothing (regex ".*/ok") Nothing RROValid] [])
                setRef
                mockRedirect
                (link "/permanent-redirect")
                (progress True)
                (VerifyResult [])
          , testCase "Do not match" $ do
              setRef <- newIORef mempty
              checkLinkAndProgressWithServer
                (configMod [RedirectRule Nothing (regex ".*/no-ok") (Just RROPermanent) RROValid] [])
                setRef
                mockRedirect
                (link "/permanent-redirect")
                (progress False)
                (VerifyResult [RedirectRuleError (chain ["/permanent-redirect", "/ok"]) (Just RROPermanent)])
          ]
      , testGroup "By \"from\""
          [ testCase "Do match" $ do
              setRef <- newIORef mempty
              checkLinkAndProgressWithServer
                (configMod [RedirectRule (regex ".*/permanent-.*") Nothing Nothing RROValid] [])
                setRef
                mockRedirect
                (link "/permanent-redirect")
                (progress True)
                (VerifyResult [])
          , testCase "Do not match" $ do
              setRef <- newIORef mempty
              checkLinkAndProgressWithServer
                (configMod [RedirectRule (regex ".*/temporary-.*") Nothing (Just RROPermanent) RROValid] [])
                setRef
                mockRedirect
                (link "/permanent-redirect")
                (progress False)
                (VerifyResult [RedirectRuleError (chain ["/permanent-redirect", "/ok"]) (Just RROPermanent)])
          ]
      , testGroup "By \"from\", \"to\" and \"on\""
        [ testCase "Do match" $ do
            setRef <- newIORef mempty
            checkLinkAndProgressWithServer
              (configMod [RedirectRule (regex ".*/follow[0-9]") (regex "^.*/ok$") (Just (RROCode 307)) RROInvalid] [])
              setRef
              mockRedirect
              (link "/follow3")
              (progress False)
              (VerifyResult [RedirectRuleError (chain ["/follow3", "/ok"]) (Just (RROCode 307))])
        , testCase "Do not match" $ do
            setRef <- newIORef mempty
            checkLinkAndProgressWithServer
              (configMod [RedirectRule (regex ".*/follow[0-9]") (regex "^.*/ok$") (Just (RROCode 307)) RROInvalid] [])
              setRef
              mockRedirect
              (link "/follow2")
              (progress True)
              (VerifyResult [])
        ]
      , testCase "By any" $ do
          setRef <- newIORef mempty
          checkLinkAndProgressWithServer
            (configMod [RedirectRule Nothing Nothing Nothing RROValid] [])
            setRef
            mockRedirect
            (link "/follow1")
            (progress True)
            (VerifyResult [])
      ]
  , testGroup "Chain"
      [ testCase "End valid" $ do
          setRef <- newIORef mempty
          checkLinkAndProgressWithServer
            (configMod [RedirectRule Nothing Nothing Nothing RROFollow] [])
            setRef
            mockRedirect
            (link "/follow1")
            (progress True)
            (VerifyResult [])
      , testCase "End invalid" $ do
          setRef <- newIORef mempty
          checkLinkAndProgressWithServer
            (configMod [RedirectRule Nothing Nothing (Just (RROCode 307)) RROInvalid, RedirectRule Nothing Nothing Nothing RROFollow] [])
            setRef
            mockRedirect
            (link "/follow1")
            (progress False)
            (VerifyResult [RedirectRuleError (chain ["/follow1", "/follow2", "/follow3", "/ok"]) (Just (RROCode 307))])
      , testCase "Mixed with ignore" $ do
          setRef <- newIORef mempty
          checkLinkAndProgressWithServer
            (configMod [RedirectRule Nothing Nothing (Just (RROCode 307)) RROInvalid, RedirectRule Nothing Nothing Nothing RROFollow] (maybeToList (regex ".*/follow3")))
            setRef
            mockRedirect
            (link "/follow1")
            (progress True)
            (VerifyResult [])
      ]
  ]
  where
    link :: Text -> Text
    link = ("http://127.0.0.1:5000" <>)

    chain :: [Text] -> RedirectChain
    chain = fromList . fmap link

    regex :: Text -> Maybe R.Regex
    regex = rightToMaybe . R.compile defaultCompOption defaultExecOption

    configMod :: [RedirectRule] -> [R.Regex] -> Config -> Config
    configMod rules exclussions config = config
      & cNetworkingL . ncExternalRefRedirectsL %~ (rules <>)
      & cExclusionsL . ecIgnoreExternalRefsToL .~ exclussions

    setHeader :: HeaderName -> Text -> Web.ActionM ()
    setHeader hdr value = Web.setHeader (decodeUtf8 (CI.original hdr)) (fromStrict value)

    progress :: Bool -> Progress Int Text
    progress shouldSucceed = report "" $ initProgress 1
      where
        report =
          if shouldSucceed
          then reportSuccess
          else reportError

    mockRedirect :: IO ()
    mockRedirect =
      Web.run 5000 <=< Web.scottyApp $ do
        Web.matchAny "/ok" $ Web.raw "Ok"
        Web.matchAny "/permanent-redirect" $ do
          setHeader hLocation "/ok"
          Web.status movedPermanently301
        Web.matchAny "/temporary-redirect" $ do
          setHeader hLocation "/ok"
          Web.status found302
        Web.matchAny "/follow1" $ do
          setHeader hLocation "/follow2"
          Web.status movedPermanently301
        Web.matchAny "/follow2" $ do
          setHeader hLocation "/follow3"
          Web.status found302
        Web.matchAny "/follow3" $ do
          setHeader hLocation "/ok"
          Web.status temporaryRedirect307
