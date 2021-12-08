{- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.ReservedURICharsSpec where

import Universum

import Test.Hspec (Spec, describe, it, shouldBe)

import Xrefcheck.Util (searchBrackets)

spec :: Spec
spec =
  describe "Reserved square brackets should be recognized if present in the URI" $ do
    it "No square brackets" $
      forM_ noSB $ \link ->
        searchBrackets link `shouldBe` []
    it "One square bracket per link" $
      forM_ oneSB $ \link ->
        searchBrackets link `shouldBe` findSB (toString link)
    it "Many square brackets in every link" $
      forM_ manySB $ \link ->
        searchBrackets link `shouldBe` findSB (toString link)
  where
    noSB =
      [ "https://example.com/"
      , "https://example.com/whatever/codata?data=co&universal=eliminator"
      , "https://example.com/enter/shikari/alexandra/palace"
      ]
    oneSB =
      [ "https://example[.com/"
      , "https://example.com/whatever/codata?data]=co&universal=eliminator"
      , "https://example.com/enter/shi[kari/alexandra/palace"
      ]
    manySB =
      [ "https://examp[le[.c]o]m/"
      , "https://example.com/whatever/codata?[data]=[co]&[universal]=[eliminator]"
      , "https://example.com/ent[[[[er/sh[]]][i[kari/a]]]lexandra/]palace"
      ]

    findSB :: String -> [Int]
    findSB = map fst . filter (\(_, ch) -> ch `elem` ("[]" :: String)) . zip [0..]
