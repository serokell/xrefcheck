{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.TrailingSlashSpec where

import Universum

import Fmt (blockListF, pretty, unlinesF)
import System.Directory (doesFileExist)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

import Xrefcheck.Config
import Xrefcheck.Core
import Xrefcheck.Progress
import Xrefcheck.Scan
import Xrefcheck.Scanners.Markdown

test_slash :: TestTree
test_slash = testGroup "Trailing forward slash detection" $
  let config = defConfig GitHub
      format = specificFormatsSupport [markdownSupport (scMarkdown (cScanners config))]
  in roots <&> \root ->
    testCase ("All the files within the root \"" <>
      root <>
      "\" should exist") $ do
        (ScanResult _ (RepoInfo repoInfo _)) <- allowRewrite False $ \rw ->
          scanRepo rw format TraversalConfig{ tcIgnored = [] } root
        nonExistentFiles <- lefts <$> forM (keys repoInfo) (\filePath -> do
          predicate <- doesFileExist filePath
          return $ if predicate
                    then Right ()
                    else Left filePath)
        if null nonExistentFiles
        then pass
        else assertFailure $ pretty $ unlinesF
          [ "Expected all filepaths to be valid, but these filepaths do not exist:"
          , blockListF nonExistentFiles
          ]
  where
    roots :: [FilePath]
    roots =
      [ "tests/markdowns/without-annotations"
      , "tests/markdowns/without-annotations/"
      , "tests/markdowns/without-annotations/./"
      ]
