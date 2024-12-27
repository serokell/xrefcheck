{- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Xrefcheck.Command
  ( defaultAction
  ) where

import Universum

import Data.Reflection (Given, give)
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Fmt (build, fmt, fmtLn)
import System.Console.Pretty (supportsPretty)
import System.Directory (doesFileExist)
import Text.Interpolation.Nyan

import Xrefcheck.CLI (Options (..), addExclusionOptions, addNetworkingOptions, defaultConfigPaths)
import Xrefcheck.Config
  (Config, Config' (..), ScannersConfig, ScannersConfig' (..), defConfig, overrideConfig)
import Xrefcheck.Core (Flavor (..))
import Xrefcheck.Progress (allowRewrite)
import Xrefcheck.Scan
import Xrefcheck.Scanners.Markdown (markdownSupport)
import Xrefcheck.Scanners.Symlink (symlinkSupport)
import Xrefcheck.System (PrintUnixPaths (..), askWithinCI)
import Xrefcheck.Util
import Xrefcheck.Verify (reportVerifyErrs, verifyErrors, verifyRepo)

readConfig :: FilePath -> IO Config
readConfig path = fmap overrideConfig do
  decodeFileEither path
    >>= either (error . toText . prettyPrintParseException) pure

configuredFileSupport :: Given PrintUnixPaths => ScannersConfig -> FileSupport
configuredFileSupport ScannersConfig{..} = firstFileSupport
    [ markdownSupport scMarkdown
    , symlinkSupport
    ]

findFirstExistingFile :: [FilePath] -> IO (Maybe FilePath)
findFirstExistingFile = \case
  [] -> pure Nothing
  (file : files) -> do
    exists <- doesFileExist file
    if exists then pure (Just file) else findFirstExistingFile files

defaultAction :: Options -> IO ()
defaultAction Options{..} = do
  withinCI <- askWithinCI
  coloringSupported <- supportsPretty
  let colorMode = oColorMode ?:
        if withinCI || coloringSupported
        then WithColors
        else WithoutColors

  give oPrintUnixPaths $ give colorMode $ do
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

    let showProgressBar = oShowProgressBar ?: not withinCI

    (ScanResult scanErrs repoInfo) <- allowRewrite showProgressBar $ \rw -> do
      let fullConfig = addExclusionOptions (cExclusions config) oExclusionOptions
          fileSupport = configuredFileSupport $ cScanners config
      scanRepo oScanPolicy rw fileSupport fullConfig oRoot

    when oVerbose $
      fmt [int||
      === Repository data ===

      #{interpolateIndentF 2 (build repoInfo)}
      |]

    whenJust (nonEmpty $ sortBy (compare `on` seFile) scanErrs) reportScanErrs

    verifyRes <- allowRewrite showProgressBar $ \rw -> do
      let fullConfig = config
            { cNetworking = addNetworkingOptions (cNetworking config) oNetworkingOptions }
      verifyRepo rw fullConfig oMode repoInfo

    case verifyErrors verifyRes of
      Nothing | null scanErrs ->
        fmtLn $ colorIfNeeded Green "All repository links are valid."
      Nothing -> exitFailure
      Just verifyErrs -> do
        unless (null scanErrs) $ fmt "\n"
        reportVerifyErrs verifyErrs
        exitFailure
