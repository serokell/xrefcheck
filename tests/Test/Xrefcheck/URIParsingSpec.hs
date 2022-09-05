{- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE QuasiQuotes #-}

module Test.Xrefcheck.URIParsingSpec where

import Universum

import Test.Hspec (Spec, describe, it, shouldReturn)
import Text.URI (URI)
import Text.URI.QQ (uri)

import Xrefcheck.Verify (parseUri, VerifyError (..))

spec :: Spec
spec = do
  describe "URI parsing should be successful" $ do
    it "Without the special characters in the query strings" do
      parseUri' "https://example.com/?q=a&p=b#fragment" `shouldReturn`
        Right [uri|https://example.com/?q=a&p=b#fragment|]

      parseUri' "https://example.com/path/to/smth?q=a&p=b" `shouldReturn`
        Right [uri|https://example.com/path/to/smth?q=a&p=b|]

    it "With the special characters in the query strings" do
      parseUri' "https://example.com/?q=[a]&<p>={b}#fragment" `shouldReturn`
        Right [uri|https://example.com/?q=%5Ba%5D&%3Cp%3E=%7Bb%7D#fragment|]

      parseUri' "https://example.com/path/to/smth?q=[a]&<p>={b}" `shouldReturn`
        Right [uri|https://example.com/path/to/smth?q=%5Ba%5D&%3Cp%3E=%7Bb%7D|]

  describe "URI parsing should be unsuccessful" $ do
    it "With the special characters anywhere else" do
      parseUri' "https://exa<mple.co>m/?q=a&p=b#fra{g}ment" `shouldReturn`
        Left ExternalResourceInvalidUri

      parseUri' "https://example.com/pa[t]h/to[/]smth?q=a&p=b" `shouldReturn`
        Left ExternalResourceInvalidUri
  where
    parseUri' :: Text -> IO $ Either VerifyError URI
    parseUri' = runExceptT . parseUri
