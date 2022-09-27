{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.IgnoreAnnotationsSpec where

import Universum

import CMarkGFM (PosInfo (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Test.Xrefcheck.Util
import Xrefcheck.Core
import Xrefcheck.Scan
import Xrefcheck.Scanners.Markdown

test_ignoreAnnotations :: [TestTree]
test_ignoreAnnotations =
  [ testGroup "Parsing failures"
      [ testCase "Check if broken link annotation produce error" do
          let file = "tests/markdowns/with-annotations/no_link.md"
          errs <- getErrs file
          errs @?= makeError (Just $ PosInfo 7 1 7 31) file Link ""
      , testCase "Check if broken paragraph annotation produce error" do
          let file = "tests/markdowns/with-annotations/no_paragraph.md"
          errs <- getErrs file
          errs @?= makeError (Just $ PosInfo 7 1 7 35) file Paragraph "HEADING"
      , testCase "Check if broken ignore file annotation produce error" do
          let file = "tests/markdowns/with-annotations/unexpected_ignore_file.md"
          errs <- getErrs file
          errs @?= makeError (Just $ PosInfo 9 1 9 30) file File ""
      , testCase "Check if broken unrecognised annotation produce error" do
          let file = "tests/markdowns/with-annotations/unrecognised_option.md"
          errs <- getErrs file
          errs @?= makeError (Just $ PosInfo 7 1 7 46) file None "unrecognised-option"
      ]
  , testGroup "\"ignore link\" mode"
      [ testCase "Check \"ignore link\" performance" $ do
          fi <- getFI GitHub "tests/markdowns/with-annotations/ignore_link.md"
          getRefs fi @?=
            ["team", "team", "team", "hire-us", "how-we-work", "privacy", "link2", "link2"]
      ]
  , testGroup "\"ignore paragraph\" mode"
      [ testCase "Check \"ignore paragraph\" performance" $ do
         fi <- getFI GitHub "tests/markdowns/with-annotations/ignore_paragraph.md"
         getRefs fi @?= ["blog", "contacts"]
      ]
  , testGroup "\"ignore file\" mode"
      [ testCase "Check \"ignore file\" performance" $ do
        fi <- getFI GitHub "tests/markdowns/with-annotations/ignore_file.md"
        getRefs fi @?= []
      ]
  ]
  where
    getRefs :: FileInfo -> [Text]
    getRefs fi = map rName $ fi ^. fiReferences

    getErrs :: FilePath -> IO [ScanError]
    getErrs path = snd <$> parse GitHub path
