module Main where

import Data.Yaml (decodeFileEither)
import Fmt (blockListF', build, fmt, fmtLn, indentF, (+|), (|+))
import System.Directory (doesFileExist)
import System.FilePath.Posix (pathSeparator)

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

main :: IO ()
main = do
    Options{..} <- getOptions
    let root = oRoot

    configExists <- doesFileExist oConfig
    unless configExists $
      if oConfig == defaultConfigPath
      then die $ "No configuration file found!\n\n\
                 \Consider using default configuration from repository with the tool\n\
                 \> crossref-verify --config <crossref-verifier-repo>"
                    <> [pathSeparator] <> defaultConfigPath <> "\n\
                 \or adding configuration file to your repository."
      else die $ "Configuration file '" +| oConfig |+ "' does not exists"
    config <- decodeFileEither oConfig
              >>= either (error . show) pure

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
