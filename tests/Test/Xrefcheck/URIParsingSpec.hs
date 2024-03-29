{- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.URIParsingSpec where

import Universum

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.URI (URI)
import Text.URI.QQ (uri)
import URI.ByteString qualified as URIBS

import Xrefcheck.Data.URI (UriParseError (..), parseUri)

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
            (@?= Left (UPEInvalid URIBS.MalformedPath))

          parseUri' "https://example.com/pa[t]h/to[/]smth?q=a&p=b" >>=
            (@?= Left (UPEInvalid URIBS.MalformedPath))

      , testCase "With malformed scheme" do
          parseUri' "https//example.com/" >>=
            (@?= Left (UPEInvalid $ URIBS.MalformedScheme URIBS.MissingColon))

      , testCase "With malformed fragment" do
          parseUri' "https://example.com/?q=a&p=b#fra{g}ment" >>=
            (@?= Left (UPEInvalid URIBS.MalformedFragment))
      ]
  ]
  where
    parseUri' :: Text -> IO $ Either UriParseError URI
    parseUri' = runExceptT . parseUri False
