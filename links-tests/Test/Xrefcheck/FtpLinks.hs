-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.Xrefcheck.FtpLinks
  ( FtpHostOpt(..)
  , ftpOptions
  , test_FtpLinks
  ) where

import Universum

import Data.Tagged (Tagged, untag)
import Options.Applicative (help, long, strOption)
import Test.Tasty (TestTree, askOption, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.Options as Tasty (IsOption (..), OptionDescription (Option), safeRead)

import Xrefcheck.Config (Config (cVerification), VerifyConfig (vcCheckLocalhost), defConfig)
import Xrefcheck.Core (Flavor (GitHub))
import Xrefcheck.Verify
  (VerifyError (..), VerifyResult (VerifyResult), checkExternalResource, verifyErrors)

-- | A list with all the options needed to configure FTP links tests.
ftpOptions :: [OptionDescription]
ftpOptions =
  [ Tasty.Option (Proxy @FtpHostOpt)
  ]

-- | Option specifying FTP host.
newtype FtpHostOpt = FtpHostOpt Text
  deriving (Show, Eq)

instance IsOption FtpHostOpt where
  defaultValue = FtpHostOpt "ftp://localhost"
  optionName = "ftp-host"
  optionHelp = "[Test.Xrefcheck.FtpLinks] FTP host without trailing slash"
  parseValue v = FtpHostOpt <$> safeRead v
  optionCLParser = FtpHostOpt <$> strOption
    (  long (untag (optionName :: Tagged FtpHostOpt String))
    <> help (untag (optionHelp :: Tagged FtpHostOpt String))
    )


config :: VerifyConfig
config = (cVerification $ defConfig GitHub) { vcCheckLocalhost = True }

test_FtpLinks :: TestTree
test_FtpLinks = askOption $ \(FtpHostOpt host) -> do
  testGroup "Ftp links handler"
    [ testCase "handles correct link to file" $ do
        let link = host <> "/pub/file_exists.txt"
        result <- checkExternalResource config link
        result @?= VerifyResult []

    , testCase "handles empty link (host only)" $ do
        let link = host
        result <- checkExternalResource config link
        result @?= VerifyResult []

    , testCase "handles correct link to non empty directory" $ do
        let link = host <> "/pub/"
        result <- checkExternalResource config link
        result @?= VerifyResult []

    , testCase "handles correct link to empty directory" $ do
        let link = host <> "/empty/"
        result <- checkExternalResource config link
        result @?= VerifyResult []

    , testCase "throws exception when file not found" $ do
        let link = host <> "/pub/file_does_not_exists.txt"
        result <- checkExternalResource config link
        case verifyErrors result of
          Nothing ->
            assertFailure "No exception was raised, FtpEntryDoesNotExist expected"
          Just errors ->
            assertBool "Expected FtpEntryDoesNotExist, got other exceptions"
            (any (
              \case
                FtpEntryDoesNotExist _ -> True
                ExternalFtpException _ -> True
                _ -> False
            ) $ toList errors)
    ]
