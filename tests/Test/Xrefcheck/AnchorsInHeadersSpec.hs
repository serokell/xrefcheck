{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.AnchorsInHeadersSpec where

import Universum

import Test.Hspec (Spec, describe, it, shouldBe)

import Xrefcheck.Core
import Test.Xrefcheck.Util

spec :: Spec
spec = do
    describe "Anchors in headers" $ do
        it "Check if anchors in headers are recognized" $ do
            fi <- getFI GitHub "tests/markdowns/without-annotations/anchors_in_headers.md"
            getAnchors fi `shouldBe` ["some-stuff", "stuff-section"]

        it "Check if anchors with id attributes are recognized" $ do
            fi <- getFI GitHub "tests/markdowns/without-annotations/anchors_in_headers_with_id_attribute.md"
            getAnchors fi `shouldBe` ["some-stuff-with-id-attribute", "stuff-section-with-id-attribute"]
    where
        getAnchors :: FileInfo -> [Text]
        getAnchors fi = map aName $ fi ^. fiAnchors
