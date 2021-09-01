{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.AnchorsSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck ((===))

import Test.Xrefcheck.Util
import Xrefcheck.Core

checkHeaderConversions
  :: HasCallStack
  => Flavor -> [(Text, Text)] -> Spec
checkHeaderConversions fl suites =
  describe (show fl) $ do
    forM_ suites $ \(a, b) ->
      it (show a <> " == " <> show b) $ headerToAnchor fl a === b
    it "Non-stripped header name should be stripped" $ do
      fi <- getFI fl "tests/markdowns/without-annotations/non_stripped_spaces.md"
      getAnchors fi `shouldBe` [ case fl of GitHub -> "header--with-leading-spaces"
                                            GitLab -> "header-with-leading-spaces"
                               ]
  where
    getAnchors :: FileInfo -> [Text]
    getAnchors fi = map aName $ fi ^. fiAnchors

spec :: Spec
spec = do
  describe "Header-to-anchor conversion" $ do
    checkHeaderConversions GitHub
      [ ( "Some header"
        , "some-header"
        )
      , ( "Do +5 times"
        , "do-5-times"
        )
      , ( "a # b"
        , "a--b"
        )
      , ( "a ## b"
        , "a--b"
        )
      , ( "a - b"
        , "a---b"
        )
      , ( "a * b"
        , "a--b"
        )
      , ( "a / b"
        , "a--b"
        )
      , ( "a \\ b"
        , "a--b"
        )
      , ( "a + b"
        , "a--b"
        )
      , ( "a+b"
        , "ab"
        )
      , ( "a & b"
        , "a--b"
        )
      , ( "a &b"
        , "a-b"
        )
      , ( "a - -- - b"
        , "a--------b"
        )
      , ( "a -+--|- b"
        , "a------b"
        )
      , ( "Some *italic* text"
        , "some-italic-text"
        )
      , ( "-Some-text-with--many----hyphens-"
        , "-some-text-with--many----hyphens-"
        )
      , ( "- A -"
        , "--a--"
        )
      , ( "Some-+++++--mess++-mda"
        , "some---mess-mda"
        )
      , ( ":white_check_mark: Checklist for your Pull Request"
        , "white_check_mark-checklist-for-your-pull-request"
        )
      ]

    checkHeaderConversions GitLab
      [ ( "a # b"
        , "a-b"
        )
      , ( "a - b"
        , "a-b"
        )
      , ( "a -- b"
        , "a-b"
        )
      , ( "a & b"
        , "a-b"
        )
      , ( "a + b"
        , "a-b"
        )
      , ( "a+b"
        , "ab"
        )
      , ( "a - -- - b"
        , "a-b"
        )
      , ( "a -+--|- b"
        , "a-b"
        )
      , ( "Some *italic* text"
        , "some-italic-text"
        )
      , ( "-Some-text-with--many----hyphens-"
        , "-some-text-with-many-hyphens-"
        )
      , ( "- A -"
        , "-a-"
        )
      , ( "Some-+++++--mess++-mda"
        , "some-mess-mda"
        )
      , ( ":white_check_mark: Checklist for your Pull Request"
        , "white_check_mark-checklist-for-your-pull-request"
        )
      ]
