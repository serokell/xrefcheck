{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.AnchorsSpec where

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck ((===))

import Xrefcheck.Core (Flavor (..), headerToAnchor)

checkHeaderConversions
  :: HasCallStack
  => Flavor -> [(Text, Text)] -> Spec
checkHeaderConversions fl suites =
  describe (show fl) $
    forM_ suites $ \(a, b) ->
      it (show a <> " == " <> show b) $ headerToAnchor fl a === b

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
