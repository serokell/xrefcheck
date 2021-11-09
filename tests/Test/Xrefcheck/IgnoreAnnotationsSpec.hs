{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.IgnoreAnnotationsSpec where

import Universum

import Test.Hspec (Spec, describe, it, shouldBe)

import Test.Xrefcheck.Util
import Xrefcheck.Core

spec :: Spec
spec = do
  describe "Parsing failures" $ do
    it "Check if parsing incorrect markdowns produce exceptions" $ do
      areIncorrect <- mapM (isIncorrectMD GitHub) failPaths
      or areIncorrect `shouldBe` True
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
    failPaths :: [FilePath]
    failPaths =
      [ "tests/markdowns/with-annotations/no_link.md"
      , "tests/markdowns/with-annotations/no_paragraph.md"
      , "tests/markdowns/with-annotations/unexpected_ignore_file.md"
      , "tests/markdowns/with-annotations/unrecognised_option.md"
      ]

    getRefs :: FileInfo -> [Text]
    getRefs fi = map rName $ fi ^. fiReferences

    isIncorrectMD :: Flavor -> FilePath -> IO Bool
    isIncorrectMD fl path = do
      errOrInfo <- parse fl path
      return $ isLeft errOrInfo
