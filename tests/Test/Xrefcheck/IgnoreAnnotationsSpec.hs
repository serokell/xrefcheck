{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.IgnoreAnnotationsSpec where

import Universum hiding ((^.))

import CMarkGFM (PosInfo (..))
import Control.Lens ((^.))
import System.FilePath qualified as FP
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Test.Xrefcheck.Util
import Xrefcheck.Core
import Xrefcheck.Scan
import Xrefcheck.Scanners.Markdown
import Xrefcheck.System

test_ignoreAnnotations :: [TestTree]
test_ignoreAnnotations =
  [ testGroup "Parsing failures"
      [ testCase "Check if broken link annotation produce error" do
          let file = "tests" FP.</> "markdowns" FP.</> "with-annotations" FP.</> "no_link.md"
          errs <- getErrs file
          errs @?= makeError file (Just $ PosInfo 7 1 7 31) LinkErr
      , testCase "Check if broken paragraph annotation produce error" do
          let file = "tests" FP.</> "markdowns" FP.</> "with-annotations" FP.</> "no_paragraph.md"
          errs <- getErrs file
          errs @?= makeError file (Just $ PosInfo 7 1 7 35) (ParagraphErr "HEADING")
      , testCase "Check if broken ignore all annotation produce error" do
          let file = "tests" FP.</> "markdowns" FP.</> "with-annotations" FP.</> "unexpected_ignore_file.md"
          errs <- getErrs file
          errs @?= makeError file (Just $ PosInfo 9 1 9 29) FileErr
      , testCase "Check if broken unrecognised annotation produce error" do
          let file = "tests" FP.</> "markdowns" FP.</> "with-annotations" FP.</> "unrecognised_option.md"
          errs <- getErrs file
          errs @?= makeError file (Just $ PosInfo 7 1 7 46) (UnrecognisedErr "unrecognised-option")
      ]
  , testGroup "\"ignore link\" mode"
      [ testCase "Check \"ignore link\" performance" $ do
          let file = "tests" FP.</> "markdowns" FP.</> "with-annotations" FP.</> "ignore_link.md"
          (fi, errs) <- parse GitHub "" (mkRelPosixLink file)
          getRefs fi @?=
            ["team", "team", "team", "hire-us", "how-we-work", "privacy", "link2", "link2", "link3"]
          errs @?= makeError file (Just $ PosInfo 42 1 42 31) LinkErr
      ]
  , testGroup "\"ignore paragraph\" mode"
      [ testCase "Check \"ignore paragraph\" performance" $ do
          let file = mkRelPosixLink $ "tests" FP.</> "markdowns" FP.</> "with-annotations" FP.</> "ignore_paragraph.md"
          (fi, errs) <- parse GitHub "" file
          getRefs fi @?= ["blog", "contacts"]
          errs @?= []
      ]
  , testGroup "\"ignore all\" mode"
      [ testCase "Check \"ignore all\" performance" $ do
          let file = mkRelPosixLink $ "tests" FP.</> "markdowns" FP.</> "with-annotations" FP.</> "ignore_file.md"
          (fi, errs) <- parse GitHub "" file
          getRefs fi @?= []
          errs @?= []
      ]
  ]
  where
    getRefs :: FileInfo -> [Text]
    getRefs fi = map rName $ fi ^. fiReferences

    getErrs :: FilePath -> IO [ScanError 'Parse]
    getErrs path = snd <$> parse GitHub "" (mkRelPosixLink path)
