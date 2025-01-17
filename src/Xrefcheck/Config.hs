{- SPDX-FileCopyrightText: 2018-2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Xrefcheck.Config
  ( module Xrefcheck.Config
  , module Xrefcheck.Data.Redirect
  , defConfigText
  ) where

import Universum hiding ((.~))

import Control.Lens (makeLensesWith, (.~))
import Data.Aeson (genericParseJSON)
import Data.Yaml (FromJSON (..), decodeEither', prettyPrintParseException, withText)
import Text.Regex.TDFA.Text ()
import Time (KnownRatName, Second, Time (..), unitsP)

import Xrefcheck.Config.Default
import Xrefcheck.Core
import Xrefcheck.Data.Redirect
import Xrefcheck.Scan
import Xrefcheck.Scanners.Markdown
import Xrefcheck.Util (Field, aesonConfigOption, postfixFields)

-- | Type alias for Config' with all required fields.
type Config = Config' Identity

-- | Type alias for Config' with optional fields.
type ConfigOptional = Config' Maybe

-- | Overall config.
data Config' f = Config
  { cExclusions :: Field f (ExclusionConfig' f)
  , cNetworking :: Field f (NetworkingConfig' f)
  , cScanners   :: ScannersConfig' f
  } deriving stock (Generic)

-- | Type alias for NetworkingConfig' with all required fields.
type NetworkingConfig = NetworkingConfig' Identity

-- | Config of networking.
data NetworkingConfig' f = NetworkingConfig
  { ncExternalRefCheckTimeout   :: Field f (Time Second)
    -- ^ When checking external references, how long to wait on request before
    -- declaring "Response timeout".
  , ncIgnoreAuthFailures        :: Field f Bool
    -- ^ If True - links which return 403 or 401 code will be skipped,
    -- otherwise – will be marked as broken, because we can't check it.
  , ncDefaultRetryAfter         :: Field f (Time Second)
    -- ^ Default Retry-After delay, applicable when we receive a 429 response
    -- and it does not contain a @Retry-After@ header.
  , ncMaxRetries                :: Field f Int
    -- ^ How many attempts to retry an external link after getting
    -- a "429 Too Many Requests" response.
    -- Timeouts may also be accounted here, see the description
    -- of `maxTimeoutRetries` field.
    --
    -- If a site once responded with 429 error code, subsequent
    -- request timeouts will also be treated as hitting the site's
    -- rate limiter and result in retry attempts, unless the
    -- maximum retries number has been reached.
    --
    -- On other errors xrefcheck fails immediately, without retrying.
  , ncMaxTimeoutRetries         :: Field f Int
    -- ^ Querying a given domain that ever returned 429 before,
    -- this defines how many timeouts are allowed during retries.
    --
    -- For such domains, timeouts likely mean hitting the rate limiter,
    -- and so xrefcheck considers timeouts in the same way as 429 errors.
    --
    -- For other domains, a timeout results in a respective error, no retry
    -- attempts will be performed. Use `externalRefCheckTimeout` option
    -- to increase the time after which timeout is declared.
    --
    -- This option is similar to `maxRetries`, the difference is that
    -- this `maxTimeoutRetries` option limits only the number of retries
    -- caused by timeouts, and `maxRetries` limits the number of retries
    -- caused both by 429s and timeouts.
  , ncMaxRedirectFollows        :: Field f Int
    -- ^ Maximum number of links that can be followed in a single redirect
    -- chain.
  , ncExternalRefRedirects      :: Field f RedirectConfig
    -- ^  Rules to override the redirect behavior for external references.
  } deriving stock (Generic)

-- | A list of custom redirect rules.
type RedirectConfig = [RedirectRule]

-- | Type alias for ScannersConfig' with all required fields.
type ScannersConfig = ScannersConfig' Identity

-- | Configs for all the supported scanners.
data ScannersConfig' f = ScannersConfig
  { scMarkdown :: MarkdownConfig
  , scAnchorSimilarityThreshold :: Field f Double
    -- ^ On 'anchor not found' error, how much similar anchors should be displayed as
    -- hint. Number should be between 0 and 1, larger value means stricter filter.
  } deriving stock (Generic)

makeLensesWith postfixFields ''Config'
makeLensesWith postfixFields ''NetworkingConfig'

defConfig :: HasCallStack => Flavor -> Config
defConfig = either (error . toText . prettyPrintParseException) id
    . decodeEither'
    . encodeUtf8
    . defConfigText

-- | Override missed fields with default values.
overrideConfig :: ConfigOptional -> Config
overrideConfig config
  = Config
    { cExclusions = maybe defExclusions overrideExclusions $ cExclusions config
    , cNetworking = maybe defNetworking overrideNetworking $ cNetworking config
    , cScanners = ScannersConfig
                  { scMarkdown = MarkdownConfig flavor
                  , scAnchorSimilarityThreshold =
                    fromMaybe (scAnchorSimilarityThreshold defScanners)
                    $ scAnchorSimilarityThreshold (cScanners config)
                  }
    }
  where
    flavor = mcFlavor . scMarkdown $ cScanners config

    defScanners = cScanners $ defConfig flavor
    defExclusions = cExclusions $ defConfig flavor
    defNetworking = cNetworking (defConfig flavor)
      & ncExternalRefRedirectsL .~ []

    overrideExclusions exclusionConfig
      = ExclusionConfig
        { ecIgnore               = overrideField ecIgnore
        , ecIgnoreLocalRefsTo    = overrideField ecIgnoreLocalRefsTo
        , ecIgnoreRefsFrom       = overrideField ecIgnoreRefsFrom
        , ecIgnoreExternalRefsTo = overrideField ecIgnoreExternalRefsTo
        }
      where
        overrideField :: (forall f. ExclusionConfig' f -> Field f a) -> a
        overrideField field = fromMaybe (field defExclusions) $ field exclusionConfig

    overrideNetworking networkingConfig
      = NetworkingConfig
        { ncExternalRefCheckTimeout   = overrideField ncExternalRefCheckTimeout
        , ncIgnoreAuthFailures        = overrideField ncIgnoreAuthFailures
        , ncDefaultRetryAfter         = overrideField ncDefaultRetryAfter
        , ncMaxRetries                = overrideField ncMaxRetries
        , ncMaxTimeoutRetries         = overrideField ncMaxTimeoutRetries
        , ncMaxRedirectFollows        = overrideField ncMaxRedirectFollows
        , ncExternalRefRedirects      = overrideField ncExternalRefRedirects
        }
      where
        overrideField :: (forall f. NetworkingConfig' f -> Field f a) -> a
        overrideField field = fromMaybe (field defNetworking) $ field networkingConfig

-----------------------------------------------------------
-- Yaml instances
-----------------------------------------------------------

instance KnownRatName unit => FromJSON (Time unit) where
  parseJSON = withText "time" $
    maybe (fail "Unknown time") pure . unitsP . toString

instance FromJSON (ConfigOptional) where
  parseJSON = genericParseJSON aesonConfigOption

instance FromJSON (Config) where
  parseJSON = genericParseJSON aesonConfigOption

instance FromJSON (NetworkingConfig' Maybe) where
  parseJSON = genericParseJSON aesonConfigOption

instance FromJSON (NetworkingConfig) where
  parseJSON = genericParseJSON aesonConfigOption

instance FromJSON (ScannersConfig) where
  parseJSON = genericParseJSON aesonConfigOption

instance FromJSON (ScannersConfig' Maybe) where
  parseJSON = genericParseJSON aesonConfigOption
