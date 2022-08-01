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
  , normaliseWithNoTrailing
  , posixTimeToTimeSecond
  , utcTimeToTimeSecond
  ) where

import Universum

import Control.Lens (LensRules, lensField, lensRules, mappingNamer)
import Data.Aeson qualified as Aeson
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Fixed (Fixed (MkFixed), HasResolution (resolution))
import Data.Ratio ((%))
import Data.Time (UTCTime)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Fmt (Builder, build, fmt, nameF)
import System.Console.Pretty (Pretty (..), Style (Faint))
import System.FilePath (dropTrailingPathSeparator, normalise)
import Time (Second, Time (..), sec)

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

normaliseWithNoTrailing :: FilePath -> FilePath
normaliseWithNoTrailing =  dropTrailingPathSeparator . normalise

posixTimeToTimeSecond :: POSIXTime -> Time Second
posixTimeToTimeSecond posixTime =
  let picos@(MkFixed ps) = nominalDiffTimeToSeconds posixTime
  in sec . fromRational $ ps % resolution picos

utcTimeToTimeSecond :: UTCTime -> Time Second
utcTimeToTimeSecond = posixTimeToTimeSecond . utcTimeToPOSIXSeconds
