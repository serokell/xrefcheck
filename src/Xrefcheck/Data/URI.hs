{- SPDX-FileCopyrightText: 2023 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE ExistentialQuantification #-}

module Xrefcheck.Data.URI
  ( UriParseError (..)
  , parseUri
  ) where

import Universum

import Control.Exception.Safe (handleJust)
import Control.Monad.Except (throwError)
import Text.URI (ParseExceptionBs, URI, mkURIBs)
import URI.ByteString qualified as URIBS

data UriParseError
  = UPEInvalid URIBS.URIParseError
  | UPEConversion ParseExceptionBs
  deriving stock (Show, Eq)

data AnyURIRef = forall a. AnyURIRef (URIBS.URIRef a)

serializeAnyURIRef :: AnyURIRef -> ByteString
serializeAnyURIRef (AnyURIRef uri) = URIBS.serializeURIRef' uri

-- | Parse URI according to RFC 3986 extended by allowing non-encoded
-- `[` and `]` in query string.
--
-- The first parameter indicates whether the parsing should admit relative
-- URIs or not.
parseUri :: Bool -> Text -> ExceptT UriParseError IO URI
parseUri canBeRelative link = do
  -- There exist two main standards of URL parsing: RFC 3986 and the Web
  -- Hypertext Application Technology Working Group's URL standard. Ideally,
  -- we want to be able to parse the URLs in accordance with the latter
  -- standard, because it provides a much less ambiguous set of rules for
  -- percent-encoding special characters, and is essentially a living
  -- standard that gets updated constantly.
  --
  -- We have chosen the 'uri-bytestring' library for URI parsing because
  -- of the 'laxURIParseOptions' parsing configuration. 'mkURI' from
  -- the 'modern-uri' library parses URIs in accordance with RFC 3986 and does
  -- not provide a means of parsing customization, which contrasts with
  -- 'parseURI' that accepts a 'URIParserOptions'. One of the predefined
  -- configurations of this type is 'strictURIParserOptions', which follows
  -- RFC 3986, and the other -- 'laxURIParseOptions' -- allows brackets
  -- in the queries, which draws us closer to the WHATWG URL standard.
  --
  -- The 'modern-uri' package can parse an URI deciding if it is absolute or
  -- relative depending on the success or failure of the scheme parsing. By
  -- contrast, in 'uri-bytestring' it has to be decided beforehand, resulting in
  -- different URI types.
  uri <- case URIBS.parseURI URIBS.laxURIParserOptions (encodeUtf8 link) of
    Left (URIBS.MalformedScheme _) | canBeRelative ->
      URIBS.parseRelativeRef URIBS.laxURIParserOptions (encodeUtf8 link)
        & either (throwError . UPEInvalid) (pure . AnyURIRef)
    Left err -> throwError $ UPEInvalid err
    Right uri -> pure $ AnyURIRef uri

  -- We stick to our infrastructure by continuing to operate on the datatypes
  -- from 'modern-uri', which are used in the 'req' library. First we
  -- serialize our URI parsed with 'parseURI' so it becomes a 'ByteString'
  -- with all the necessary special characters *percent-encoded*, and then
  -- call 'mkURIBs'.
  mkURIBs (serializeAnyURIRef uri)
    -- Ideally, this exception should never be thrown, as the URI
    -- already *percent-encoded* with 'parseURI' from 'uri-bytestring'
    -- and 'mkURIBs' is only used to convert to 'URI' type from
    -- 'modern-uri' package.
    & handleJust fromException (throwError . UPEConversion)
