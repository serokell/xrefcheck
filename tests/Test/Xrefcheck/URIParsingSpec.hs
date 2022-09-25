{- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE QuasiQuotes #-}

module Test.Xrefcheck.URIParsingSpec where

import Universum

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.URI (URI)
import Text.URI.QQ (uri)
import URI.ByteString (SchemaError (..), URIParseError (..))

import Xrefcheck.Verify (VerifyError (..), parseUri)

test_uri :: [TestTree]
test_uri =
  [ testGroup "URI parsing should be successful"
      [ testCase "Without the special characters in the query strings" do
          parseUri' "https://example.com/?q=a&p=b#fragment" >>=
            (@?= Right [uri|https://example.com/?q=a&p=b#fragment|])
          parseUri' "https://example.com/path/to/smth?q=a&p=b" >>=
            (@?= Right [uri|https://example.com/path/to/smth?q=a&p=b|])

      , testCase "With the special characters in the query strings" do
          parseUri' "https://example.com/?q=[a]&<p>={b}#fragment" >>=
            (@?= Right
              [uri|https://example.com/?q=%5Ba%5D&%3Cp%3E=%7Bb%7D#fragment|])

          parseUri' "https://example.com/path/to/smth?q=[a]&<p>={b}" >>=
            (@?= Right
              [uri|https://example.com/path/to/smth?q=%5Ba%5D&%3Cp%3E=%7Bb%7D|])
      ]
  , testGroup "URI parsing should be unsuccessful"
      [ testCase "With the special characters anywhere else" do
          parseUri' "https://exa<mple.co>m/?q=a&p=b#fra{g}ment" >>=
            (@?= Left (ExternalResourceInvalidUri MalformedPath))

          parseUri' "https://example.com/pa[t]h/to[/]smth?q=a&p=b" >>=
            (@?= Left (ExternalResourceInvalidUri MalformedPath))

      , testCase "With malformed scheme" do
          parseUri' "https//example.com/" >>=
            (@?= Left (ExternalResourceInvalidUri $ MalformedScheme MissingColon))

      , testCase "With malformed fragment" do
          parseUri' "https://example.com/?q=a&p=b#fra{g}ment" >>=
            (@?= Left (ExternalResourceInvalidUri MalformedFragment))
      ]
  ]
  where
    parseUri' :: Text -> IO $ Either VerifyError URI
    parseUri' = runExceptT . parseUri
