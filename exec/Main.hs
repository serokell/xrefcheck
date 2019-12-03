module Main where

import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Fmt (blockListF', build, fmt, fmtLn, indentF)

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

    config <- case oConfigPath of
      Nothing -> pure defConfig
      Just configPath ->
        decodeFileEither configPath
        >>= either (error . toText . prettyPrintParseException) pure

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
