{- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Xrefcheck.Command
  ( defaultAction
  ) where

import Universum

import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Fmt (blockListF', build, fmt, fmtLn, indentF)
import System.Directory (doesFileExist)

import Xrefcheck.CLI (Options (..), addTraversalOptions, addVerifyOptions, defaultConfigPaths)
import Xrefcheck.Config (Config (..), ScannersConfig (..), defConfig, normaliseConfigFilePaths)
import Xrefcheck.Core (Flavor (..))
import Xrefcheck.Progress (allowRewrite)
import Xrefcheck.Scan (FormatsSupport, gatherRepoInfo, specificFormatsSupport)
import Xrefcheck.Scanners.Markdown (markdownSupport)
import Xrefcheck.System (askWithinCI)
import Xrefcheck.Verify (verifyErrors, verifyRepo)

readConfig :: FilePath -> IO Config
readConfig path = fmap normaliseConfigFilePaths do
  decodeFileEither path
    >>= either (error . toText . prettyPrintParseException) pure

formats :: ScannersConfig -> FormatsSupport
formats ScannersConfig{..} = specificFormatsSupport
    [ markdownSupport scMarkdown
    ]

findFirstExistingFile :: [FilePath] -> IO (Maybe FilePath)
findFirstExistingFile = \case
  [] -> pure Nothing
  (file : files) -> do
    exists <- doesFileExist file
    if exists then pure (Just file) else findFirstExistingFile files

defaultAction :: Options -> IO ()
defaultAction Options{..} = do
    config <- case oConfigPath of
      Just configPath -> readConfig configPath
      Nothing -> do
        mConfigPath <- findFirstExistingFile defaultConfigPaths
        case mConfigPath of
          Just configPath -> readConfig configPath
          Nothing -> do
            hPutStrLn @Text stderr
              "Configuration file not found, using default config \
              \for GitHub repositories\n"
            pure $ defConfig GitHub

    withinCI <- askWithinCI
    let showProgressBar = oShowProgressBar ?: not withinCI

    repoInfo <- allowRewrite showProgressBar $ \rw -> do
      let fullConfig = addTraversalOptions (cTraversal config) oTraversalOptions
      gatherRepoInfo rw (formats $ cScanners config) fullConfig oRoot

    when oVerbose $
      fmtLn $ "=== Repository data ===\n\n" <> indentF 2 (build repoInfo)

    verifyRes <- allowRewrite showProgressBar $ \rw -> do
      let fullConfig = addVerifyOptions (cVerification config) oVerifyOptions
      verifyRepo rw fullConfig oMode oRoot repoInfo

    case verifyErrors verifyRes of
      Nothing ->
        fmtLn "All repository links are valid."
      Just (toList -> errs) -> do
        fmt $ "=== Invalid references found ===\n\n" <>
              indentF 2 (blockListF' "➥ " build errs)
        fmtLn $ "Invalid references dumped, " <> build (length errs) <> " in total."
        exitFailure
