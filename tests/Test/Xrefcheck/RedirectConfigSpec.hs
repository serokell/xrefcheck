{- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.RedirectConfigSpec where

import Universum

import Data.CaseInsensitive qualified as CI
import Data.Map qualified as M
import Network.HTTP.Types (mkStatus)
import Network.HTTP.Types.Header (hLocation)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Text.Regex.TDFA.Text qualified as R
import Web.Firefly (App, Status, ToResponse (toResponse), route, run)

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
                (progress 1)
                (VerifyResult [RedirectRuleError (chain ["/temporary-redirect", "/ok"]) (Just RROTemporary)])
          , testCase "Do not match" $ do
              setRef <- newIORef mempty
              checkLinkAndProgressWithServer
                (configMod [RedirectRule Nothing Nothing (Just RROPermanent) RROInvalid] [])
                setRef
                mockRedirect
                (link "/temporary-redirect")
                (progress 0)
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
                (progress 0)
                (VerifyResult [])
          , testCase "Do not match" $ do
              setRef <- newIORef mempty
              checkLinkAndProgressWithServer
                (configMod [RedirectRule Nothing (regex ".*/no-ok") (Just RROPermanent) RROValid] [])
                setRef
                mockRedirect
                (link "/permanent-redirect")
                (progress 1)
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
                (progress 0)
                (VerifyResult [])
          , testCase "Do not match" $ do
              setRef <- newIORef mempty
              checkLinkAndProgressWithServer
                (configMod [RedirectRule (regex ".*/temporary-.*") Nothing (Just RROPermanent) RROValid] [])
                setRef
                mockRedirect
                (link "/permanent-redirect")
                (progress 1)
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
              (progress 1)
              (VerifyResult [RedirectRuleError (chain ["/follow3", "/ok"]) (Just (RROCode 307))])
        , testCase "Do not match" $ do
            setRef <- newIORef mempty
            checkLinkAndProgressWithServer
              (configMod [RedirectRule (regex ".*/follow[0-9]") (regex "^.*/ok$") (Just (RROCode 307)) RROInvalid] [])
              setRef
              mockRedirect
              (link "/follow2")
              (progress 0)
              (VerifyResult [])
        ]
      , testCase "By any" $ do
          setRef <- newIORef mempty
          checkLinkAndProgressWithServer
            (configMod [RedirectRule Nothing Nothing Nothing RROValid] [])
            setRef
            mockRedirect
            (link "/follow1")
            (progress 0)
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
            (progress 0)
            (VerifyResult [])
      , testCase "End invalid" $ do
          setRef <- newIORef mempty
          checkLinkAndProgressWithServer
            (configMod [RedirectRule Nothing Nothing (Just (RROCode 307)) RROInvalid, RedirectRule Nothing Nothing Nothing RROFollow] [])
            setRef
            mockRedirect
            (link "/follow1")
            (progress 1)
            (VerifyResult [RedirectRuleError (chain ["/follow1", "/follow2", "/follow3", "/ok"]) (Just (RROCode 307))])
      , testCase "Mixed with ignore" $ do
          setRef <- newIORef mempty
          checkLinkAndProgressWithServer
            (configMod [RedirectRule Nothing Nothing (Just (RROCode 307)) RROInvalid, RedirectRule Nothing Nothing Nothing RROFollow] (maybeToList (regex ".*/follow3")))
            setRef
            mockRedirect
            (link "/follow1")
            (progress 0)
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

    status :: Int -> Status
    status code = mkStatus code "Redirect"

    configMod :: [RedirectRule] -> [R.Regex] -> Config -> Config
    configMod rules exclussions config = config
      & cNetworkingL . ncExternalRefRedirectsL %~ (rules <>)
      & cExclusionsL . ecIgnoreExternalRefsToL .~ exclussions

    redirectRoute :: Text -> Int -> Maybe Text -> App ()
    redirectRoute name code to = route name $ pure $ toResponse
      ( "" :: Text
      , status code
      , M.fromList [(CI.map (decodeUtf8 @Text) hLocation, fmap link $ maybeToList to)]
      )

    progress :: Int -> Progress Int
    progress errors = Progress
      { pTotal = 1
      , pCurrent = 1
      , pErrorsUnfixable = errors
      , pErrorsFixable = 0
      , pTaskTimestamp = Nothing
      }

    mockRedirect :: IO ()
    mockRedirect =
      run 5000 do
        route "/ok" $ pure $ toResponse ("Ok" :: Text)
        redirectRoute "/permanent-redirect" 301 $ Just "/ok"
        redirectRoute "/temporary-redirect" 302 $ Just "/ok"
        redirectRoute "/follow1" 301 $ Just "/follow2"
        redirectRoute "/follow2" 302 $ Just "/follow3"
        redirectRoute "/follow3" 307 $ Just "/ok"
