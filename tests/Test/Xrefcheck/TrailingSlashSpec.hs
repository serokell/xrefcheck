{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.TrailingSlashSpec where

import Universum

import Fmt (blockListF, pretty, unlinesF)
import System.Directory (doesFileExist)
import Test.Hspec (Spec, describe, expectationFailure, it)

import Xrefcheck.Config
import Xrefcheck.Core
import Xrefcheck.Progress
import Xrefcheck.Scan
import Xrefcheck.Scanners.Markdown

spec :: Spec
spec = do
  describe "Trailing forward slash detection" $ do
    let config = defConfig GitHub
    let format = specificFormatsSupport [markdownSupport (scMarkdown (cScanners config))]
    forM_ roots $ \root -> do
      it ("All the files within the root \"" <>
          root <>
          "\" should exist") $ do
        RepoInfo repoInfo <- allowRewrite False $ \rw ->
          gatherRepoInfo rw format TraversalConfig{ tcIgnored = [] } root
        nonExistentFiles <- lefts <$> forM (keys repoInfo) (\filePath -> do
          predicate <- doesFileExist filePath
          return $ if predicate
                   then Right ()
                   else Left filePath)
        if null nonExistentFiles
        then pass
        else expectationFailure $ pretty $ unlinesF
          [ "Expected all filepaths to be valid, but these filepaths do not exist:"
          , blockListF nonExistentFiles
          ]
  where
    roots :: [FilePath]
    roots =
      [ "tests/markdowns/without-annotations"
      , "tests/markdowns/without-annotations/"
      , "tests/markdowns/without-annotations/./"
      , "tests/markdowns/without-annotations/all_checked.md"
      , "tests/markdowns/without-annotations/./all_checked.md"
      ]
