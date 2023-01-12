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
  , doesMatchAnyRegex
  , posixTimeToTimeSecond
  , utcTimeToTimeSecond

  , module Xrefcheck.Util.Colorize
  , module Xrefcheck.Util.Interpolate
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
import Text.Regex.TDFA.Text (Regex, regexec)
import Time (Second, Time (..), sec)

import Xrefcheck.Util.Colorize
import Xrefcheck.Util.Interpolate

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

posixTimeToTimeSecond :: POSIXTime -> Time Second
posixTimeToTimeSecond posixTime =
  let picos@(MkFixed ps) = nominalDiffTimeToSeconds posixTime
  in sec . fromRational $ ps % resolution picos

utcTimeToTimeSecond :: UTCTime -> Time Second
utcTimeToTimeSecond = posixTimeToTimeSecond . utcTimeToPOSIXSeconds

doesMatchAnyRegex :: Text -> ([Regex] -> Bool)
doesMatchAnyRegex src = any $ \regex ->
  case regexec regex src of
    Right res -> case res of
      Just (before, match, after, _) ->
        null before && null after && not (null match)
      Nothing -> False
    Left _ -> False
