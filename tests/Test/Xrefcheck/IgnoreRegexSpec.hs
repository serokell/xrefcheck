{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.IgnoreRegexSpec where

import Data.Yaml (decodeEither')
import Test.HUnit (assertFailure)
import Test.Hspec (Spec, describe, it)
import Text.Regex.TDFA (Regex)

import Xrefcheck.Config
import Xrefcheck.Core
import Xrefcheck.Progress (allowRewrite)
import Xrefcheck.Scan (gatherRepoInfo, specificFormatsSupport)
import Xrefcheck.Scanners.Markdown
import Xrefcheck.Verify (VerifyError, VerifyResult, WithReferenceLoc (..), verifyErrors, verifyRepo)

spec :: Spec
spec = do
    describe "Regular expressions performance" $ do
        let root = "tests/markdowns/without-annotations"
        let showProgressBar = False
        let formats = specificFormatsSupport [markdownSupport defGithubMdConfig]
        let verifyMode = ExternalOnlyMode

        let linksTxt =
                [ "https://bad.((external.)?)reference(/?)"
                , "https://bad.reference.(org|com)"
                ]
        let regexs = linksToRegexs linksTxt
        let config = setIgnoreRefs regexs (defConfig GitHub)

        it "Check that only not matched links are verified" $ do
            repoInfo <- allowRewrite showProgressBar $ \rw ->
                gatherRepoInfo rw formats (config ^. cTraversalL) root

            verifyRes <- allowRewrite showProgressBar $ \rw ->
                verifyRepo rw (config ^. cVerificationL) verifyMode root repoInfo

            let brokenLinks = pickBrokenLinks verifyRes

            let matchedLinks =
                    [ "https://bad.referenc/"
                    , "https://bad.reference"
                    , "https://bad.external.reference/"
                    , "https://bad.external.reference"
                    , "https://bad.reference.org"
                    , "https://bad.reference.com"
                    ]

            let notMatchedLinks =
                    [ "https://non-existent.reference/"
                    , "https://bad.externall.reference"
                    , "https://bad.reference.io"
                    ]

            forM_ matchedLinks $ \link -> do
                when (link `elem` brokenLinks) $
                    assertFailure $ "Link \"" <> show link <>
                                    "\" is considered as broken but it should be ignored"

            forM_ notMatchedLinks $ \link -> do
                when (link `notElem` brokenLinks) $
                    assertFailure $ "Link \"" <> show link <>
                                    "\" is not considered as broken but it is (and shouldn't be ignored)"

    where
        pickBrokenLinks :: VerifyResult (WithReferenceLoc VerifyError) -> [Text]
        pickBrokenLinks verifyRes =
            case verifyErrors verifyRes of
                Just neWithRefLoc -> map (rLink . wrlReference) $ toList neWithRefLoc
                Nothing -> []

        linksToRegexs :: [Text] -> Maybe [Regex]
        linksToRegexs links =
            let errOrRegexs = map (decodeEither' . encodeUtf8) links
                maybeRegexs = map (either (error . show) Just) errOrRegexs
            in sequence maybeRegexs

        setIgnoreRefs :: Maybe [Regex] -> Config -> Config
        setIgnoreRefs regexs = (cVerificationL . vcIgnoreRefsL) .~ regexs
