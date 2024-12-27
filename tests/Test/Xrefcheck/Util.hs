{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.Util where

import Universum

import Data.Reflection (give)
import Data.Tagged (untag)
import Network.HTTP.Types (forbidden403, unauthorized401)
import Network.Wai.Handler.Warp qualified as Web
import Options.Applicative (auto, help, long, option)
import Test.Tasty.Options as Tasty (IsOption (..), OptionDescription (Option), safeRead)
import Web.Scotty qualified as Web

import Xrefcheck.Core (Flavor)
import Xrefcheck.Scan (ScanAction)
import Xrefcheck.Scanners.Markdown (MarkdownConfig (MarkdownConfig, mcFlavor), markdownScanner)
import Xrefcheck.System (PrintUnixPaths (..))

parse :: Flavor -> ScanAction
parse fl path =
  give (PrintUnixPaths False) $
  markdownScanner MarkdownConfig { mcFlavor = fl } path

mockServerUrl :: MockServerPort -> Text -> Text
mockServerUrl (MockServerPort port) s = toText ("http://127.0.0.1:" <> show port <> s)

mockServer :: MockServerPort -> IO ()
mockServer (MockServerPort port) =
  Web.run port <=< Web.scottyApp $ do
    Web.matchAny "/401" $ Web.status unauthorized401
    Web.matchAny "/403" $ Web.status forbidden403

-- | All options needed to configure the mock server.
mockServerOptions :: [OptionDescription]
mockServerOptions =
  [ Tasty.Option (Proxy @MockServerPort)
  ]

-- | Option specifying FTP host.
newtype MockServerPort = MockServerPort Int
  deriving stock (Show, Eq)

instance IsOption MockServerPort where
  defaultValue = MockServerPort 3000
  optionName = "mock-server-port"
  optionHelp = "[Test.Xrefcheck.Util] Mock server port"
  parseValue v = MockServerPort <$> safeRead v
  optionCLParser = MockServerPort <$> option auto
    (  long (untag @MockServerPort optionName)
    <> help (untag @MockServerPort optionHelp)
    )
