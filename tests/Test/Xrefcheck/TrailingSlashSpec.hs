{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.TrailingSlashSpec where

import Universum hiding ((.~))

import Control.Lens ((.~))
import System.Directory (doesFileExist)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Text.Interpolation.Nyan

import Xrefcheck.Config
import Xrefcheck.Core
import Xrefcheck.Progress
import Xrefcheck.Scan
import Xrefcheck.Scanners.Markdown
import Xrefcheck.System
import Xrefcheck.Util

test_slash :: TestTree
test_slash = testGroup "Trailing forward slash detection" $
  let config = defConfig GitHub
      fileSupport = firstFileSupport [markdownSupport (scMarkdown (cScanners config))]
  in roots <&> \root ->
    testCase ("All the files within the root \"" <>
      root <>
      "\" should exist") $ do
        (ScanResult _ RepoInfo{..}) <- allowRewrite False $ \rw ->
          scanRepo OnlyTracked rw fileSupport (cExclusions config & ecIgnoreL .~ []) root
        nonExistentFiles <- lefts <$> forM (fst . snd <$> toPairs riFiles) (\file -> do
          predicate <- doesFileExist . filePathFromRoot root $ file
          return $ if predicate
                    then Right ()
                    else Left . filePathFromRoot root $ file)
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
