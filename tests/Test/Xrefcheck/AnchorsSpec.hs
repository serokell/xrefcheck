{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.AnchorsSpec where

import Universum

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Test.Xrefcheck.Util
import Xrefcheck.Core
import Xrefcheck.System

checkHeaderConversions :: Flavor -> [(Text, Text)] -> TestTree
checkHeaderConversions fl suites =
  testGroup (show fl) $
    [testCase (show a <> " == " <> show b) $ headerToAnchor fl a @?= b | (a,b) <- suites]
    ++
    [ testCase "Non-stripped header name should be stripped" $ do
        (fi, errs) <- parse fl "" $ mkRelPosixLink "tests/markdowns/without-annotations/non_stripped_spaces.md"
        getAnchors fi @?= [ case fl of GitHub -> "header--with-leading-spaces"
                                       GitLab -> "header-with-leading-spaces"
                          , "edge-case"
                          ]
        errs @?= []
    ]
  where
    getAnchors :: FileInfo -> [Text]
    getAnchors fi = map aName $ fi ^. fiAnchors

test_anchors :: TestTree
test_anchors = do
  testGroup "Header-to-anchor conversion"
    [ checkHeaderConversions GitHub
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
    , checkHeaderConversions GitLab
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
    ]
