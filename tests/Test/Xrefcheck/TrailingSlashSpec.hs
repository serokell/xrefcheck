{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.TrailingSlashSpec where

import Universum

import System.Directory (doesFileExist)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Text.Interpolation.Nyan

import Xrefcheck.Config
import Xrefcheck.Core
import Xrefcheck.Progress
import Xrefcheck.Scan
import Xrefcheck.Scanners.Markdown
import Xrefcheck.Util

test_slash :: TestTree
test_slash = testGroup "Trailing forward slash detection" $
  let config = defConfig GitHub
      format = specificFormatsSupport [markdownSupport (scMarkdown (cScanners config))]
  in roots <&> \root ->
    testCase ("All the files within the root \"" <>
      root <>
      "\" should exist") $ do
        (ScanResult _ (RepoInfo repoInfo _)) <- allowRewrite False $ \rw ->
          scanRepo rw format (cExclusions config & ecIgnoreL .~ []) root
        nonExistentFiles <- lefts <$> forM (keys repoInfo) (\filePath -> do
          predicate <- doesFileExist filePath
          return $ if predicate
                    then Right ()
                    else Left filePath)
        whenJust (nonEmpty nonExistentFiles) $ \files ->
          assertFailure
            [int||
            Expected all filepaths to be valid, but these filepaths do not exist:
            #{interpolateBlockListF files}
            |]
  where
    roots :: [FilePath]
    roots =
      [ "tests/markdowns/without-annotations"
      , "tests/markdowns/without-annotations/"
      , "tests/markdowns/without-annotations/./"
      ]
