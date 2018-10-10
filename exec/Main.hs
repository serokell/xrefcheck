module Main where

import Data.Default (def)
import Fmt (blockListF, build, fmtLn, nameF)

import Crv.Scan
import Crv.Scanners
import Crv.Verify

formats :: FormatsSupport
formats = specificFormatsSupport
    [ markdownSupport
    ]

main :: IO ()
main = do
    let root = "../universum"
    repoInfo <- gatherRepoInfo formats def root
    fmtLn $ nameF "Repository links" $ build repoInfo

    verifyRes <- verifyRepo root repoInfo
    case verifyErrors verifyRes of
        Nothing ->
            fmtLn "All repository links are valid"
        Just (toList -> errs) -> do
            fmtLn $ nameF "Invalid references found" $ blockListF errs
            die ""
