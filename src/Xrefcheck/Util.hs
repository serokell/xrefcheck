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
  , module Xrefcheck.Util.Interpolate
  , printTimeSince, initialTime, printTimestamps) where

import Universum

import Control.Lens (LensRules, lensField, lensRules, mappingNamer)
import Data.Aeson qualified as Aeson
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Fixed (Fixed (MkFixed), HasResolution (resolution))
import Data.Ratio ((%))
import Data.Time (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Fmt (Builder)
import System.FilePath.Posix (dropTrailingPathSeparator, normalise)
import Time (Second, Time (..), sec)

import Data.Text qualified as T
import GHC.IO (unsafePerformIO)
import Text.Printf (printf)
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

normaliseWithNoTrailing :: FilePath -> FilePath
normaliseWithNoTrailing =  dropTrailingPathSeparator . normalise

posixTimeToTimeSecond :: POSIXTime -> Time Second
posixTimeToTimeSecond posixTime =
  let picos@(MkFixed ps) = nominalDiffTimeToSeconds posixTime
  in sec . fromRational $ ps % resolution picos

utcTimeToTimeSecond :: UTCTime -> Time Second
utcTimeToTimeSecond = posixTimeToTimeSecond . utcTimeToPOSIXSeconds

initialTime :: UTCTime
{-# NOINLINE initialTime #-}
initialTime = unsafePerformIO getCurrentTime

timePrints :: IORef [(Text, Text)]
{-# NOINLINE timePrints #-}
timePrints = unsafePerformIO $ newIORef []

printTimeSince :: Text -> IO ()
printTimeSince descr = do
  t1 <- getCurrentTime
  atomicModifyIORef' timePrints $ \lst -> do
    let time = (printf "time: %.2fs" (realToFrac (diffUTCTime t1 initialTime) :: Double)) :: String
    ((descr, T.pack time) : lst, ())

printTimestamps :: IO ()
printTimestamps = do
  entries <- reverse <$> readIORef timePrints
  forM_ entries $ \(descr, time) ->
    hPutStrLn stderr $ descr <> "; " <> time
