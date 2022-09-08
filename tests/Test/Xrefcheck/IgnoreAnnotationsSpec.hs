{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.IgnoreAnnotationsSpec where

import Universum

import CMarkGFM (PosInfo (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

import Test.Xrefcheck.Util
import Xrefcheck.Core
import Xrefcheck.Scan
import Xrefcheck.Scanners.Markdown

spec :: Spec
spec = do
  describe "Parsing failures" $ do
    it "Check if broken link annotation produce error" do
      let file = "tests/markdowns/with-annotations/no_link.md"
      getErrs file `shouldReturn`
        makeError (Just $ PosInfo 7 1 7 31) file Link ""
    it "Check if broken paragraph annotation produce error" do
      let file = "tests/markdowns/with-annotations/no_paragraph.md"
      getErrs file `shouldReturn`
        makeError (Just $ PosInfo 7 1 7 35) file Paragraph "HEADING"
    it "Check if broken ignore file annotation produce error" do
      let file = "tests/markdowns/with-annotations/unexpected_ignore_file.md"
      getErrs file `shouldReturn`
        makeError (Just $ PosInfo 9 1 9 30) file File ""
    it "Check if broken unrecognised annotation produce error" do
      let file = "tests/markdowns/with-annotations/unrecognised_option.md"
      getErrs file `shouldReturn`
        makeError (Just $ PosInfo 7 1 7 46) file None "unrecognised-option"
  describe "\"ignore link\" mode" $ do
    it "Check \"ignore link\" performance" $ do
      fi <- getFI GitHub "tests/markdowns/with-annotations/ignore_link.md"
      getRefs fi `shouldBe`
        ["team", "team", "team", "hire-us", "how-we-work", "privacy", "link2", "link2"]
  describe "\"ignore paragraph\" mode" $ do
    it "Check \"ignore paragraph\" performance" $ do
      fi <- getFI GitHub "tests/markdowns/with-annotations/ignore_paragraph.md"
      getRefs fi `shouldBe` ["blog", "contacts"]
  describe "\"ignore file\" mode" $ do
    it "Check \"ignore file\" performance" $ do
      fi <- getFI GitHub "tests/markdowns/with-annotations/ignore_file.md"
      getRefs fi `shouldBe` []
  where
    getRefs :: FileInfo -> [Text]
    getRefs fi = map rName $ fi ^. fiReferences

    getErrs :: FilePath -> IO [ScanError]
    getErrs path = snd <$> parse GitHub path
