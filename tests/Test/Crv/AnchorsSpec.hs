module Test.Crv.AnchorsSpec where

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck ((===))

import Crv.Core (headerToAnchor)

spec :: Spec
spec = do
    describe "Header-to-anchor conversion" $
        mapM_ (\(a, b) -> it (show a <> " == " <> show b) $ headerToAnchor a === b) $
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
        , ( "a * b"
          , "a--b"
          )
        , ( "a / b"
          , "a--b"
          )
        , ( "a \\ b"
          , "a--b"
          )
        , ( "Some *italic* text"
          , "some-italic-text"
          )
        , ( "Some-text-with--many----hyphens"
          , "some-text-with--many----hyphens"
          )
        , ( "Some-+++++--mess++-mda"
          , "some---mess-mda"
          )
        , ( ":white_check_mark: Checklist for your Pull Request"
          , "white_check_mark-checklist-for-your-pull-request"
          )
        ]
