{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.Util where

import qualified Data.ByteString.Lazy as BSL

import Xrefcheck.Core
import Xrefcheck.Scanners.Markdown

parse :: Flavor -> FilePath -> IO (Either Text FileInfo)
parse fl path =
  parseFileInfo MarkdownConfig{ mcFlavor = fl } . decodeUtf8 <$> BSL.readFile path

getFI :: Flavor -> FilePath -> IO FileInfo
getFI fl path =
  let errOrFI = parse fl path
  in either error id <$> errOrFI
