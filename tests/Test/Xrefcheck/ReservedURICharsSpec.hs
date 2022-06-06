{- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.ReservedURICharsSpec where

import Universum

import Test.Hspec (Spec, describe, it, shouldBe)

import Xrefcheck.Util (searchReservedChars)

spec :: Spec
spec =
  describe "Reserved characters, if present, should be recognized in the URI" $ do
    it "No reserved characters" $
      forM_ noSB $ \link ->
        searchReservedChars link `shouldBe` []
    it "One reserved character per link" $
      forM_ oneSB $ \link ->
        searchReservedChars link `shouldBe` findSBTrivialCase (toString link)
    it "Many reserved characters in every link" $
      forM_ manySB $ \link ->
        searchReservedChars link `shouldBe` findSBTrivialCase (toString link)
  where
    noSB =
      [ "https://example.com/"
      , "https://example.com/whatever/codata?data=co&universal=eliminator"
      , "https://example.com/enter/shikari/alexandra/palace"
      ]
    oneSB =
      [ "https://example{.com/"
      , "https://example.com/whatever/codata?data]=co&universal=eliminator"
      , "https://example.com/enter/shi>kari/alexandra/palace"
      ]
    manySB =
      [ "https://examp{le[.c]o}m/"
      , "https://example.com/whatever/codata?{data}=<co>&{universal}=[eliminator]"
      , "https://example.com/ent[[[[er/sh[]]][i[kari/a]]]lexandra/]palace"
      ]

    findSBTrivialCase :: String -> [Int]
    findSBTrivialCase = map fst . filter (\(_, ch) -> ch `elem` ("[]" :: String)) . zip [0..]
