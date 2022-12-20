{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.IgnoreRegexSpec where

import Universum

import Data.Reflection (give)
import Data.Yaml (decodeEither')
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Text.Regex.TDFA (Regex)

import Xrefcheck.Config
import Xrefcheck.Core
import Xrefcheck.Progress (allowRewrite)
import Xrefcheck.Scan (ScanResult (..), ecIgnoreExternalRefsToL, scanRepo, specificFormatsSupport)
import Xrefcheck.Scanners.Markdown
import Xrefcheck.Util (ColorMode (WithoutColors))
import Xrefcheck.Verify (VerifyError, VerifyResult, WithReferenceLoc (..), verifyErrors, verifyRepo)

test_ignoreRegex :: TestTree
test_ignoreRegex = give WithoutColors $
  let root = "tests/markdowns/without-annotations"
      showProgressBar = False
      formats = specificFormatsSupport [markdownSupport defGithubMdConfig]
      verifyMode = ExternalOnlyMode

      linksTxt =
        [ "https://bad.((external.)?)reference(/?)"
        , "https://bad.reference.(org|com)"
        ]
      regexs = linksToRegexs linksTxt
      config = setIgnoreRefs regexs (defConfig GitHub)


  in testGroup "Regular expressions performance"
    [ testCase "Check that only not matched links are verified" $ do
      scanResult <- allowRewrite showProgressBar $ \rw ->
        scanRepo OnlyTracked rw formats (config ^. cExclusionsL) root

      verifyRes <- allowRewrite showProgressBar $ \rw ->
        verifyRepo rw config verifyMode $ srRepoInfo scanResult

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
          assertFailure $
            "Link \"" <> show link <>
            "\" is considered as broken but it should be ignored"

      forM_ notMatchedLinks $ \link -> do
        when (link `notElem` brokenLinks) $
          assertFailure $
            "Link \"" <> show link <>
            "\" is not considered as broken but it is (and shouldn't be ignored)"
    ]

    where
      pickBrokenLinks :: VerifyResult (WithReferenceLoc VerifyError) -> [Text]
      pickBrokenLinks verifyRes =
        case verifyErrors verifyRes of
            Just neWithRefLoc -> map (rLink . wrlReference) $ toList neWithRefLoc
            Nothing -> []

      linksToRegexs :: [Text] -> [Regex]
      linksToRegexs links =
        let errOrRegexs = map (decodeEither' . encodeUtf8) links
        in map (either (error . show) id) errOrRegexs

      setIgnoreRefs :: [Regex] -> Config -> Config
      setIgnoreRefs regexs = (cExclusionsL . ecIgnoreExternalRefsToL) .~ regexs
