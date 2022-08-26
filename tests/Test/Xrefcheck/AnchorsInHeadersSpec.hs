{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.AnchorsInHeadersSpec where

import Universum

import Data.ByteString.Lazy qualified as BSL
import Test.Hspec (Spec, describe, it, shouldBe)

import Xrefcheck.Core
import Xrefcheck.Scanners.Markdown

spec :: Spec
spec = do
    describe "Anchors in headers" $ do
        it "Check if anchors in headers are recognized" $ do
            fi <- getFI "tests/markdowns/without-annotations/anchors_in_headers.md"
            getAnchors fi `shouldBe` ["some-stuff", "stuff-section"]

        it "Check if anchors with id attributes are recognized" $ do
            fi <- getFI "tests/markdowns/without-annotations/anchors_in_headers_with_id_attribute.md"
            getAnchors fi `shouldBe` ["some-stuff-with-id-attribute", "stuff-section-with-id-attribute"]
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
