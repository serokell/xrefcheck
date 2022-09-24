{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.Util where

import Universum

import Network.HTTP.Types (forbidden403, unauthorized401)
import Web.Firefly (ToResponse (..), route, run)

import Xrefcheck.Core (FileInfo, Flavor)
import Xrefcheck.Scan (ScanError)
import Xrefcheck.Scanners.Markdown (MarkdownConfig' (MarkdownConfig, mcFlavor), markdownScanner)

parse :: Flavor -> FilePath -> IO (FileInfo, [ScanError])
parse fl path = markdownScanner MarkdownConfig { mcFlavor = fl } path

getFI :: Flavor -> FilePath -> IO FileInfo
getFI fl path = fst <$> parse fl path

mockServer :: IO ()
mockServer = run 3000 $ do
  route "/401" $ pure $ toResponse ("" :: Text, unauthorized401)
  route "/403" $ pure $ toResponse ("" :: Text, forbidden403)
