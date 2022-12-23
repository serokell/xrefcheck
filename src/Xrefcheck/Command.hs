{- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Xrefcheck.Command
  ( defaultAction
  ) where

import Universum

import Data.Reflection (give)
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Fmt (build, fmt, fmtLn)
import System.Console.Pretty (supportsPretty)
import System.Directory (doesFileExist)
import Text.Interpolation.Nyan

import Xrefcheck.CLI (Options (..), addExclusionOptions, addNetworkingOptions, defaultConfigPaths)
import Xrefcheck.Config
  (Config, Config' (..), ScannersConfig, ScannersConfig' (..), defConfig, normaliseConfigFilePaths,
  overrideConfig)
import Xrefcheck.Core (Flavor (..))
import Xrefcheck.Progress (allowRewrite)
import Xrefcheck.Scan
  (FormatsSupport, ScanError (..), ScanResult (..), reportScanErrs, scanRepo,
  specificFormatsSupport)
import Xrefcheck.Scanners.Markdown (markdownSupport)
import Xrefcheck.System (askWithinCI)
import Xrefcheck.Util
import Xrefcheck.Verify (reportCopyPasteErrors, reportVerifyErrs, verifyErrors, verifyRepo)

readConfig :: FilePath -> IO Config
readConfig path = fmap (normaliseConfigFilePaths . overrideConfig) do
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
  coloringSupported <- supportsPretty
  give (if coloringSupported then oColorMode else WithoutColors) $ do
    config <- case oConfigPath of
      Just configPath -> readConfig configPath
      Nothing -> do
        mConfigPath <- findFirstExistingFile defaultConfigPaths
        case mConfigPath of
          Just configPath -> readConfig configPath
          Nothing -> do
            hPutStrLn @Text stderr
              [int||
              Configuration file not found, using default config \
              for GitHub repositories
              |]
            pure $ defConfig GitHub

    withinCI <- askWithinCI
    let showProgressBar = oShowProgressBar ?: not withinCI

    (ScanResult scanErrs repoInfo) <- allowRewrite showProgressBar $ \rw -> do
      let fullConfig = addExclusionOptions (cExclusions config) oExclusionOptions
      scanRepo oScanPolicy rw (formats $ cScanners config) fullConfig oRoot

    when oVerbose $
      fmt [int||
      === Repository data ===

      #{interpolateIndentF 2 (build repoInfo)}
      |]

    whenJust (nonEmpty $ sortBy (compare `on` seFile) scanErrs) $ reportScanErrs

    (verifyRes, copyPasteErrors) <- allowRewrite showProgressBar $ \rw -> do
      let fullConfig = config
            { cNetworking = addNetworkingOptions (cNetworking config) oNetworkingOptions }
      verifyRepo rw fullConfig oMode oRoot repoInfo

    whenJust (nonEmpty copyPasteErrors) $ \errs -> do
      reportCopyPasteErrors errs
      fmt "\n"
    case verifyErrors verifyRes of
      Nothing | null scanErrs -> fmtLn "All repository links are valid."
      Nothing -> exitFailure
      Just verifyErrs -> do
        unless (null scanErrs) $ fmt "\n"
        reportVerifyErrs verifyErrs
        exitFailure
