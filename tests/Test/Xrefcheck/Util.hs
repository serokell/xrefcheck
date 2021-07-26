{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.Util where

import Universum

import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Types (forbidden403, unauthorized401)
import Web.Firefly (ToResponse (..), route, run)

import Xrefcheck.Core (FileInfo, Flavor)
import Xrefcheck.Scanners.Markdown (MarkdownConfig (MarkdownConfig, mcFlavor), parseFileInfo)

parse :: Flavor -> FilePath -> IO (Either Text FileInfo)
parse fl path =
  parseFileInfo MarkdownConfig { mcFlavor = fl } . decodeUtf8 <$> BSL.readFile path

getFI :: Flavor -> FilePath -> IO FileInfo
getFI fl path =
  let errOrFI = parse fl path
  in either error id <$> errOrFI

mockServer :: IO ()
mockServer = run 3000 $ do
  route "/401" $ pure $ toResponse ("" :: Text, unauthorized401)
  route "/403" $ pure $ toResponse ("" :: Text, forbidden403)
