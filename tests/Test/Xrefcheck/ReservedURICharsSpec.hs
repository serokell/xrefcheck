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
      forM_ noRC $ \link ->
        fst (searchReservedChars link) `shouldBe` []
    it "One reserved character per link" $
      forM_ oneRC $ \link ->
        fst (searchReservedChars link) `shouldBe` findRCTrivialCase (toString link)
    it "Many reserved characters in every link" $
      forM_ manyRC $ \link ->
        fst (searchReservedChars link) `shouldBe` findRCTrivialCase (toString link)
  where
    noRC =
      [ "https://example.com/"
      , "https://example.com/whatever/codata?data=co&universal=eliminator"
      , "https://example.com/enter/shikari/alexandra/palace"
      ]
    oneRC =
      [ "https://example{.com/"
      , "https://example.com/whatever/codata?data]=co&universal=eliminator"
      , "https://example.com/enter/shi>kari/alexandra/palace"
      ]
    manyRC =
      [ "https://examp{le[.c]o}m/"
      , "https://example.com/whatever/codata?{data}=<co>&{universal}=[eliminator]"
      , "https://example.com/ent[[[[er/sh[]]][i[kari/a]]]lexandra/]palace"
      ]

    findRCTrivialCase :: String -> [Int]
    findRCTrivialCase = map fst . filter (\(_, ch) -> ch `elem` ("[]{}<>" :: String)) . zip [0..]
