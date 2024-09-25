-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.Xrefcheck.FtpLinks
  ( ftpOptions
  , test_FtpLinks
  ) where

import Universum hiding ((.~))

import Control.Lens ((.~))
import Data.Tagged (untag)
import Options.Applicative (help, long, strOption)
import Test.Tasty (TestTree, askOption, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.Options as Tasty (IsOption (..), OptionDescription (Option), safeRead)

import Xrefcheck.Config
import Xrefcheck.Core (Flavor (GitHub))
import Xrefcheck.Scan (ecIgnoreExternalRefsToL)
import Xrefcheck.Verify (VerifyError (..), checkExternalResource)

-- | A list with all the options needed to configure FTP links tests.
ftpOptions :: [OptionDescription]
ftpOptions =
  [ Tasty.Option (Proxy @FtpHostOpt)
  ]

-- | Option specifying FTP host.
newtype FtpHostOpt = FtpHostOpt Text
  deriving stock (Show, Eq)

instance IsOption FtpHostOpt where
  defaultValue = FtpHostOpt "ftp://localhost"
  optionName = "ftp-host"
  optionHelp = "[Test.Xrefcheck.FtpLinks] FTP host without trailing slash"
  parseValue v = FtpHostOpt <$> safeRead v
  optionCLParser = FtpHostOpt <$> strOption
    (  long (untag @FtpHostOpt optionName)
    <> help (untag @FtpHostOpt optionHelp)
    )

config :: Config
config = defConfig GitHub & cExclusionsL . ecIgnoreExternalRefsToL .~ []

test_FtpLinks :: TestTree
test_FtpLinks = askOption $ \(FtpHostOpt host) -> do
  testGroup "Ftp links handler"
    [ testCase "handles correct link to file" $ do
        let link = host <> "/pub/file_exists.txt"
        result <- runExceptT $ checkExternalResource emptyChain config link
        result @?= Right ()

    , testCase "handles empty link (host only)" $ do
        let link = host
        result <- runExceptT $ checkExternalResource emptyChain config link
        result @?= Right ()

    , testCase "handles correct link to non empty directory" $ do
        let link = host <> "/pub/"
        result <- runExceptT $ checkExternalResource emptyChain config link
        result @?= Right ()

    , testCase "handles correct link to empty directory" $ do
        let link = host <> "/empty/"
        result <- runExceptT $ checkExternalResource emptyChain config link
        result @?= Right ()

    , testCase "throws exception when file not found" $ do
        let link = host <> "/pub/file_does_not_exists.txt"
        result <- runExceptT $ checkExternalResource emptyChain config link
        case result of
          Right () ->
            assertFailure "No exception was raised, FtpEntryDoesNotExist expected"
          Left err ->
            assertBool "Expected FtpEntryDoesNotExist, got other exceptions" $
              case err of
                FtpEntryDoesNotExist _ -> True
                ExternalFtpException _ -> True
                _ -> False
    ]
