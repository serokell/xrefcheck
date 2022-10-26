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
          errs @?= makeError (Just $ PosInfo 7 1 7 31) file LinkErr
      , testCase "Check if broken paragraph annotation produce error" do
          let file = "tests/markdowns/with-annotations/no_paragraph.md"
          errs <- getErrs file
          errs @?= makeError (Just $ PosInfo 7 1 7 35) file (ParagraphErr "HEADING")
      , testCase "Check if broken ignore all annotation produce error" do
          let file = "tests/markdowns/with-annotations/unexpected_ignore_file.md"
          errs <- getErrs file
          errs @?= makeError (Just $ PosInfo 9 1 9 29) file FileErr
      , testCase "Check if broken unrecognised annotation produce error" do
          let file = "tests/markdowns/with-annotations/unrecognised_option.md"
          errs <- getErrs file
          errs @?= makeError (Just $ PosInfo 7 1 7 46) file (UnrecognisedErr "unrecognised-option")
      ]
  , testGroup "\"ignore link\" mode"
      [ testCase "Check \"ignore link\" performance" $ do
          let file = "tests/markdowns/with-annotations/ignore_link.md"
          (fi, errs) <- parse GitHub file
          getRefs fi @?=
            ["team", "team", "team", "hire-us", "how-we-work", "privacy", "link2", "link2", "link3"]
          errs @?= makeError (Just $ PosInfo 42 1 42 31) file LinkErr
      ]
  , testGroup "\"ignore paragraph\" mode"
      [ testCase "Check \"ignore paragraph\" performance" $ do
         (fi, errs) <- parse GitHub "tests/markdowns/with-annotations/ignore_paragraph.md"
         getRefs fi @?= ["blog", "contacts"]
         errs @?= []
      ]
  , testGroup "\"ignore all\" mode"
      [ testCase "Check \"ignore all\" performance" $ do
        (fi, errs) <- parse GitHub "tests/markdowns/with-annotations/ignore_file.md"
        getRefs fi @?= []
        errs @?= []
      ]
  ]
  where
    getRefs :: FileInfo -> [Text]
    getRefs fi = map rName $ fi ^. fiReferences

    getErrs :: FilePath -> IO [ScanError]
    getErrs path = snd <$> parse GitHub path
