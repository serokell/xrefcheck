{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.AnchorsInHeadersSpec where

import qualified Data.ByteString.Lazy as BSL
import Test.Hspec (Spec, describe, it, shouldBe)

import Xrefcheck.Core
import Xrefcheck.Scanners.Markdown

spec :: Spec
spec = do
    describe "\"ignore link\" mode" $ do
        it "Check \"ignore link\" performance" $ do
            fi <- getFI "tests/markdowns/without-annotations/anchors_in_headers.md"
            getAnchors fi `shouldBe` ["-some-stuff", "stuff-section"]
    where
        parse :: FilePath -> IO (Either Text FileInfo)
        parse path =
            parseFileInfo defGithubMdConfig . decodeUtf8 <$> BSL.readFile path

        getFI :: FilePath -> IO FileInfo
        getFI path =
            let errOrFI = parse path
            in either error id <$> errOrFI

        getAnchors :: FileInfo -> [Text]
        getAnchors fi = map aName $ fi ^. fiAnchors
