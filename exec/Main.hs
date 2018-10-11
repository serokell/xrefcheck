module Main where

import Data.Default (def)
import Fmt (blockListF', build, fmt, fmtLn, indentF)

import Crv.Scan
import Crv.Scanners
import Crv.Verify

formats :: FormatsSupport
formats = specificFormatsSupport
    [ markdownSupport
    ]

main :: IO ()
main = do
    let root = "../disciplina"
    repoInfo <- gatherRepoInfo formats def root
    fmtLn $ "Repository data:\n\n" <> indentF 2 (build repoInfo)

    verifyRes <- verifyRepo root repoInfo
    case verifyErrors verifyRes of
        Nothing ->
            fmtLn "All repository links are valid"
        Just (toList -> errs) -> do
            fmt $ "Invalid references found:\n\n" <>
                  indentF 2 (blockListF' ("➥ ") build errs)
            fmtLn "Invalid references dumped."
            exitFailure
