{- SPDX-FileCopyrightText: 2018-2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Xrefcheck.Util
  ( Field
  , paren
  , postfixFields
  , (-:)
  , aesonConfigOption
  , normaliseWithNoTrailing
  , posixTimeToTimeSecond
  , utcTimeToTimeSecond
  , module Xrefcheck.Util.Colorize
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
import Fmt (Builder)
import System.FilePath (dropTrailingPathSeparator, normalise)
import Time (Second, Time (..), sec)

import Xrefcheck.Util.Colorize

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
aesonConfigOption = (aesonPrefix camelCase){Aeson.rejectUnknownFields = True}

-- | Config fields that may be abscent.
type family Field f a where
  Field Identity a = a
  Field Maybe a = Maybe a

normaliseWithNoTrailing :: FilePath -> FilePath
normaliseWithNoTrailing =  dropTrailingPathSeparator . normalise

posixTimeToTimeSecond :: POSIXTime -> Time Second
posixTimeToTimeSecond posixTime =
  let picos@(MkFixed ps) = nominalDiffTimeToSeconds posixTime
  in sec . fromRational $ ps % resolution picos

utcTimeToTimeSecond :: UTCTime -> Time Second
utcTimeToTimeSecond = posixTimeToTimeSecond . utcTimeToPOSIXSeconds
