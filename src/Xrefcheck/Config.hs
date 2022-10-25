{- SPDX-FileCopyrightText: 2018-2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Xrefcheck.Config where

import Universum.Unsafe qualified as Unsafe

import Universum

import Control.Exception (assert)
import Control.Lens (makeLensesWith)
import Data.Aeson (genericParseJSON)
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Yaml (FromJSON (..), decodeEither', prettyPrintParseException, withText)
import Instances.TH.Lift ()
import Text.Regex.TDFA qualified as R
import Text.Regex.TDFA.ByteString ()
import Text.Interpolation.Nyan

import Time (KnownRatName, Second, Time (..), unitsP)

import Xrefcheck.Config.Default
import Xrefcheck.Core
import Xrefcheck.Scan
import Xrefcheck.Scanners.Markdown
import Xrefcheck.Util (Field, aesonConfigOption, postfixFields, (-:))

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

normaliseConfigFilePaths :: Config -> Config
normaliseConfigFilePaths Config{..}
  = Config
    { cExclusions = normaliseExclusionConfigFilePaths cExclusions
    , ..
    }

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
  } deriving stock (Generic)

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

-- | Picks raw config with @:PLACEHOLDER:<key>:@ and fills the specified fields
-- in it, picking a replacement suitable for the given key. Only strings and lists
-- of strings can be filled this way.
--
-- This will fail if any placeholder is left unreplaced, however extra keys in
-- the provided replacement won't cause any warnings or failures.
fillHoles
  :: HasCallStack
  => [(ByteString, Either ByteString [ByteString])]
  -> ByteString
  -> ByteString
fillHoles allReplacements rawConfig =
  let holesLocs = R.getAllMatches $ holeLineRegex `R.match` rawConfig
  in mconcat $ replaceHoles 0 holesLocs
  where
    showBs :: ByteString -> Text
    showBs = show @_ @Text . decodeUtf8

    holeLineRegex :: R.Regex
    holeLineRegex = R.makeRegex ("[ -]+:PLACEHOLDER:[^:]+:" :: Text)

    replacementsMap = Map.fromList allReplacements
    getReplacement key =
      Map.lookup key replacementsMap
      ?: error ("Replacement for key " <> showBs key <> " is not specified")

    pickConfigSubstring :: Int -> Int -> ByteString
    pickConfigSubstring from len = BS.take len $ BS.drop from rawConfig

    replaceHoles :: Int -> [(R.MatchOffset, R.MatchLength)] -> [ByteString]
    replaceHoles processedLen [] = one $ BS.drop processedLen rawConfig
    replaceHoles processedLen ((off, len) : locs) =
      -- in our case matches here should not overlap
      assert (off > processedLen) $
        pickConfigSubstring processedLen (off - processedLen) :
        replaceHole (pickConfigSubstring off len) ++
        replaceHoles (off + len) locs

    holeItemRegex :: R.Regex
    holeItemRegex = R.makeRegex ("(^|:)([ ]*):PLACEHOLDER:([^:]+):" :: Text)

    holeListRegex :: R.Regex
    holeListRegex = R.makeRegex ("^([ ]*-[ ]*):PLACEHOLDER:([^:]+):" :: Text)

    replaceHole :: ByteString -> [ByteString]
    replaceHole holeLine = if
      | Just [_wholeMatch, _beginning, leadingSpaces, key] <-
          R.getAllTextSubmatches <$> (holeItemRegex `R.matchM` holeLine) ->
            case getReplacement key of
              Left replacement -> [leadingSpaces, replacement]
              Right _ -> error $
                [int||
                Key #{showBs key} requires replacement with an item, \
                but list was given"
                |]

      | Just [_wholeMatch, leadingChars, key] <-
          R.getAllTextSubmatches <$> (holeListRegex `R.matchM` holeLine) ->
            case getReplacement key of
              Left _ -> error $
                [int||
                Key #{showBs key} requires replacement with a list, \
                but an item was given"
                |]
              Right [] ->
                ["[]"]
              Right replacements@(_ : _) ->
                Unsafe.init $ do
                  replacement <- replacements
                  [leadingChars, replacement, "\n"]

      | otherwise ->
          error $ "Unrecognized placeholder pattern " <> showBs holeLine

-- | Default config in textual representation.
--
-- Sometimes you cannot just use 'defConfig' because clarifying comments
-- would be lost.
defConfigText :: Flavor -> ByteString
defConfigText flavor =
  flip fillHoles defConfigUnfilled
    [
      "flavor" -: Left (show flavor)

    , "ignoreRefsFrom" -: Right $ case flavor of
        GitHub ->
          [ ".github/pull_request_template.md"
          , ".github/issue_template.md"
          , ".github/PULL_REQUEST_TEMPLATE/**/*"
          , ".github/ISSUE_TEMPLATE/**/*"
          ]
        GitLab ->
          [ ".gitlab/merge_request_templates/**/*"
          , ".gitlab/issue_templates/**/*"
          ]

    , "ignoreLocalRefsTo" -: Right $ case flavor of
        GitHub ->
          [ "../../../issues"
          , "../../../issues/*"
          , "../../../pulls"
          , "../../../pulls/*"
          ]
        GitLab ->
          [ "../../issues"
          , "../../issues/*"
          , "../../merge_requests"
          , "../../merge_requests/*"
          ]
    ]

defConfig :: HasCallStack => Flavor -> Config
defConfig flavor = normaliseConfigFilePaths $
  either (error . toText . prettyPrintParseException) id $
  decodeEither' (defConfigText flavor)

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
    defNetworking = cNetworking $ defConfig flavor

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
