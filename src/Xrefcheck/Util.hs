{- SPDX-FileCopyrightText: 2018-2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Xrefcheck.Util
  ( nameF'
  , paren
  , postfixFields
  , (-:)
  , aesonConfigOption
  , searchReservedChars
  , octet
  ) where

import Universum

import Control.Lens (LensRules, lensField, lensRules, mappingNamer)
import Data.Aeson qualified as Aeson
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Char (toUpper)
import Data.Text qualified as T
import Data.Text.Format (hex)
import Fmt (Builder, build, fmt, nameF)
import System.Console.Pretty (Pretty (..), Style (..))
import Text.Megaparsec (ParseError (..), ParseErrorBundle (..), bundleErrors)
import Text.URI (ParseException (..), URI (..), mkURI)

instance Pretty Builder where
    colorize s c = build @Text . colorize s c . fmt
    style s = build @Text . style s . fmt

nameF' :: Builder -> Builder -> Builder
nameF' a = nameF (style Faint a)

paren :: Builder -> Builder
paren a
  | a == "" = ""
  | otherwise = "(" <> a <> ")"

postfixFields :: LensRules
postfixFields = lensRules & lensField .~ mappingNamer (\n -> [n ++ "L"])

infixr 0 -:
(-:) :: a -> b -> (a, b)
(-:) = (,)

-- | Options that we use to derive JSON instances for config types.
aesonConfigOption :: Aeson.Options
aesonConfigOption = aesonPrefix camelCase

-- | Identify the positions of all the non-percent-encoded reserved characters in the URI.
searchReservedChars :: Text -> ([Int], Text, Bool)
searchReservedChars = withAccumulator 0 []
  where
    -- | At every iteration, call @mkURI@ to attempt the parsing of the link.
    -- If the attempt is successful, return the pair of the accumulated list of indices of
    -- the non-percent-encoded reserved characters and the resulting link; if not,
    -- call @reservedCharIndex@, returning the index of the first reserved character
    -- (read left to right) and the link with that character replaced by its percent-encoded
    -- octet, and call @withAccumulator@ recursively, appending the index to the accumulator.
    -- Each recursive call increments the offset by 2 as a result of replacing a character with
    -- an octet, the length of which is always equal to 3.
    withAccumulator :: Int -> [Int] -> Text -> ([Int], Text, Bool)
    withAccumulator offset indexAcc link =
      case mkURI link :: Either SomeException URI of
        Left se ->
          let (index, link', wasReplaced) = reservedCharIndex se link offset
          in
            if wasReplaced
            then withAccumulator (offset + 2) (index ++ indexAcc) link'
            else (reverse indexAcc, link, False)
        Right _uri -> (reverse indexAcc, link, True)

    reservedCharIndex :: SomeException -> Text -> Int -> ([Int], Text, Bool)
    reservedCharIndex se lnk offset = case (fromException se :: Maybe ParseException) of
      Nothing -> ([], lnk, False)
      Just pe ->
        let peBundle = (\(ParseException bundle) -> bundle) pe
            err :| _ = bundleErrors peBundle
        in case err of
          -- The value @position@ contains the location of the first spotted illegal
          -- character after parsing the link.
          TrivialError position (Just _) _ ->
            let characterInQuestion = lnk `T.index` position
                newLink = toText . replaceOnce position (octet characterInQuestion) $
                  toString lnk
            in ([position - offset], newLink, True)
          _ -> ([], lnk, False)

    replaceOnce :: Eq a => Int -> [a] -> [a] -> [a]
    replaceOnce pos replacement lst =
      let (prefix, rest) = splitAt pos lst
      in case rest of
        [] -> lst
        _ : suffix -> prefix ++ replacement ++ suffix

octet :: Char -> String
octet = ("%" <>) . map toUpper . fmt @String . hex . ord
