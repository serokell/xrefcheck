{- SPDX-FileCopyrightText: 2018-2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Main where

import qualified Data.ByteString as BS
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Fmt (blockListF', build, fmt, fmtLn, indentF)
import System.Directory (doesFileExist)

import Crv.CLI
import Crv.Config
import Crv.Progress
import Crv.Scan
import Crv.Scanners
import Crv.Verify

formats :: FormatsSupport
formats = specificFormatsSupport
    [ markdownSupport
    ]

defaultAction :: Options -> IO ()
defaultAction Options{..} = do
    let root = oRoot

    config <- case oConfigPath of
      Nothing -> do
        mConfigPath <- findFirstExistingFile defaultConfigPaths
        case mConfigPath of
          Nothing -> do
            hPutStrLn @Text stderr "Configuration file not found, using default config\n"
            pure defConfig
          Just configPath ->
            readConfig configPath
      Just configPath -> do
        readConfig configPath

    repoInfo <- allowRewrite oShowProgressBar $ \rw ->
        gatherRepoInfo rw formats (cTraversal config) root

    when oVerbose $
        fmtLn $ "Repository data:\n\n" <> indentF 2 (build repoInfo)

    verifyRes <- allowRewrite oShowProgressBar $ \rw ->
        verifyRepo rw (cVerification config) oMode root repoInfo
    case verifyErrors verifyRes of
        Nothing ->
            fmtLn "All repository links are valid."
        Just (toList -> errs) -> do
            fmt $ "Invalid references found:\n\n" <>
                  indentF 2 (blockListF' "➥ " build errs)
            fmtLn $ "Invalid references dumped, " <> build (length errs) <> " in total."
            exitFailure
  where
    findFirstExistingFile :: [FilePath] -> IO (Maybe FilePath)
    findFirstExistingFile = \case
      [] -> pure Nothing
      (file : files) -> do
        exists <- doesFileExist file
        if exists then pure (Just file) else findFirstExistingFile files

    readConfig :: FilePath -> IO Config
    readConfig path =
      decodeFileEither path
      >>= either (error . toText . prettyPrintParseException) pure

main :: IO ()
main = do
    command <- getCommand
    case command of
      DefaultCommand options ->
        defaultAction options
      DumpConfig path ->
        BS.writeFile path defConfigText
