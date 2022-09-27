{- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan instances for types from other packages

module Xrefcheck.Orphans () where

import Universum

import Data.ByteString.Char8 qualified as C

import Fmt (Buildable (..), unlinesF, (+|), (|+))
import Network.FTP.Client
  (FTPException (..), FTPMessage (..), FTPResponse (..), ResponseStatus (..))
import Text.URI (RText, unRText)
import URI.ByteString (SchemaError (..), URIParseError (..))

instance ToString (RText t) where
  toString = toString . unRText

instance Buildable ResponseStatus where
  build = show

instance Buildable FTPMessage where
  build message = build $ decodeUtf8 @Text (
    case message of
      SingleLine s -> s
      MultiLine ss -> C.intercalate "\n" ss
    )

instance Buildable FTPResponse where
  build FTPResponse{..} = unlinesF
    [ frStatus |+ " (" +| frCode |+ "):"
    , build frMessage
    ]

instance Buildable FTPException where
  build (BadProtocolResponseException _) = "Raw FTP exception"
  build (FailureRetryException e) = build e
  build (FailureException e) = build e
  build (UnsuccessfulException e) = build e
  build (BogusResponseFormatException e) = build e

deriving stock instance Eq FTPException

instance Buildable URIParseError where
  build = \case
    MalformedScheme e ->  build e
    MalformedUserInfo -> "Malformed user info"
    MalformedQuery -> "Malformed query"
    MalformedFragment -> "Malformed fragment"
    MalformedHost -> "Malformed host"
    MalformedPort -> "Malformed port"
    MalformedPath -> "Malformed path"
    OtherError e -> build e

instance Buildable SchemaError where
  build = \case
    NonAlphaLeading -> "Scheme must start with an alphabet character"
    InvalidChars -> "Subsequent characters in the schema were invalid"
    MissingColon -> "Schemas must be followed by a colon"
