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
  , invalidURIVerbose
  ) where

import Universum

import Control.Lens (LensRules, lensField, lensRules, mappingNamer)
import Data.Aeson qualified as Aeson
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Char (toUpper)
import qualified Data.Map as M
import qualified Data.Text as T (map, replicate)
import Data.Text.Format (hex)
import Fmt (Builder, build, fmt, indentF, nameF, unlinesF)
import System.Console.Pretty (Color (..), Pretty (..), Style (..))

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

invalidURIVerbose :: Char -> Int -> Text -> Text
invalidURIVerbose characterInQuestion position link = fmt . indentF 3 . unlinesF $
  pinpointInvalidCharacter <>
  suggestPercentEncoding
  where
    pinpointInvalidCharacter  =
      [ "unexpected character " <> show characterInQuestion
      , style Bold (color Magenta "|")
      , style Bold (color Magenta "|") <> "  " <>
        applyPrettyAt position link Bold Magenta
      , style Bold (color Magenta "|") <> "  " <>
        T.replicate position " " <> style Bold (color Magenta "^")
      , show characterInQuestion <>
        " is not allowed to be explcitily used in the URIs by the WHATWG URL standard"
      , "Consider using its percent-encoded counterpart: " <>
        style Bold (color Magenta $ octet characterInQuestion)
      ]

    suggestPercentEncoding = maybeToList $ do
      paired <- characterInQuestion `M.lookup` pairedEnclosingGlyphs
      return $
        "Similarly for " <> show paired <>
        " if it is present in the URI: " <>
        style Bold (color Magenta (octet paired))

    applyPrettyAt :: Int -> Text -> Style -> Color -> Text
    applyPrettyAt pos text sty col =
      let s = splitAt pos $ toString text
      in toText $ case s of
        (prefix, []) -> prefix
        ([], suffix@(h : t)) ->
          if pos < 0
          then suffix
          else style sty (color col [h]) <> t
        (prefix, h : t) -> prefix <> color col [h] <> t

    pairedEnclosingGlyphs :: M.Map Char Char
    pairedEnclosingGlyphs = M.fromList
      [ ('(', ')')
      , (')', '(')
      , ('[', ']')
      , (']', '[')
      , ('{', '}')
      , ('}', '{')
      , ('<', '>')
      , ('>', '<')
      ]

    octet :: Char -> Text
    octet = ("%" <>) . T.map toUpper . fmt @Text . hex . ord
