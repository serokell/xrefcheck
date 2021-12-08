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
  , searchBrackets
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
import Text.Megaparsec (ParseError (..), bundleErrors)
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

-- | Identify the positions of all the illegal square brackets in the URI.
searchBrackets :: Text -> [Int]
searchBrackets = withAccumulator []
  where
    -- | On every iteration, call @mkURI@ to attempt the parsing of the link.
    -- If the attempt is successful, return the accumulated list of indecies of illegal
    -- square brackets; if not, call @bracketIndex@, returning the index of the first
    -- spotted illegal bracket (read from left to right) and the link with that character
    -- replaced by '_', and call @withAccumulator@ recursively, appending the index to the
    -- accumulator.
    withAccumulator :: [Int] -> Text -> [Int]
    withAccumulator bracketIndexAcc link =
      case mkURI link :: Either SomeException URI of
        Left se -> let (index, link') = bracketIndex se link
                   in withAccumulator (index ++ bracketIndexAcc) link'
        Right _uri -> reverse bracketIndexAcc

    bracketIndex :: SomeException -> Text -> ([Int], Text)
    bracketIndex se lnk = case (fromException se :: Maybe ParseException) of
      Nothing -> ([], lnk)
      Just pe ->
        let peBundle = (\(ParseException bundle) -> bundle) pe
            err :| _ = bundleErrors peBundle
        in case err of
          -- The value @position@ contains the location of the first spotted illegal
          -- character after parsing the link.
          TrivialError position _ _ ->
            let characterInQuestion = lnk `T.index` position
                newLink = toText . replaceOnce characterInQuestion '_' $ toString lnk
            in
              -- If the spotted character is *not* a square bracket, do not include
              -- its location.
              ( [position | characterInQuestion `elem` illegalCharactersToDisplay]
              , newLink
              )
          _ -> ([], lnk)

    replaceOnce :: Eq a => a -> a -> [a] -> [a]
    replaceOnce _ _ [] = []
    replaceOnce pattern replacement (x : xs)
      | pattern == x = replacement : xs
      | otherwise = x : replaceOnce pattern replacement xs

    illegalCharactersToDisplay :: String
    illegalCharactersToDisplay = "[]"

octet :: Char -> Text
octet = ("%" <>) . T.map toUpper . fmt @Text . hex . ord
