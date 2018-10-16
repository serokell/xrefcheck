module Main where

import Data.Yaml (decodeFileEither)
import Fmt (blockListF', build, fmt, fmtLn, indentF)

import Crv.CLI
import Crv.Config
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
    config <- decodeFileEither oConfig
              >>= either (error . show) pure

    repoInfo <- gatherRepoInfo formats (cTraversal config) root
    when (cVerbose config) $
        fmtLn $ "Repository data:\n\n" <> indentF 2 (build repoInfo)

    verifyRes <- verifyRepo (cVerification config) root repoInfo
    case verifyErrors verifyRes of
        Nothing ->
            fmtLn "All repository links are valid."
        Just (toList -> errs) -> do
            fmt $ "Invalid references found:\n\n" <>
                  indentF 2 (blockListF' ("➥ ") build errs)
            fmtLn $ "Invalid references dumped, " <> build (length errs) <> " in total."
            exitFailure
