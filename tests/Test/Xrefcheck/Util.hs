{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.Util where

import Universum

import Network.HTTP.Types (forbidden403, unauthorized401)
import Web.Firefly (ToResponse (..), route, run)

import Xrefcheck.Core (FileInfo, Flavor)
import Xrefcheck.Scan (ScanError, ScanStage (..))
import Xrefcheck.Scanners.Markdown (MarkdownConfig (MarkdownConfig, mcFlavor), markdownScanner)
import Xrefcheck.System (canonicalizePath)

parse :: Flavor -> FilePath -> IO (FileInfo, [ScanError 'Parse])
parse fl path = do
  canonicalPath <- canonicalizePath path
  markdownScanner MarkdownConfig { mcFlavor = fl } canonicalPath

mockServer :: IO ()
mockServer = run 3000 $ do
  route "/401" $ pure $ toResponse ("" :: Text, unauthorized401)
  route "/403" $ pure $ toResponse ("" :: Text, forbidden403)
