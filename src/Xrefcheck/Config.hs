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
import Text.Regex.TDFA.Common
import Text.Regex.TDFA.Text qualified as R

import Time (KnownRatName, Second, Time (..), unitsP)

import Xrefcheck.Config.Default
import Xrefcheck.Core
import Xrefcheck.Scan
import Xrefcheck.Scanners.Markdown
import Xrefcheck.System (RelGlobPattern, normaliseGlobPattern)
import Xrefcheck.Util (Field, aesonConfigOption, postfixFields, (-:))

-- | Type alias for Config' with all required fields.
type Config = Config' Identity

-- | Type alias for Config' with optional fields.
type ConfigOptional = Config' Maybe

-- | Overall config.
data Config' f = Config
  { cTraversal    :: Field f (TraversalConfig' f)
  , cVerification :: Field f (VerifyConfig' f)
  , cScanners     :: ScannersConfig
  } deriving stock (Generic)

normaliseConfigFilePaths :: Config -> Config
normaliseConfigFilePaths Config{..}
  = Config
    { cTraversal = normaliseTraversalConfigFilePaths cTraversal
    , cVerification = normaliseVerifyConfigFilePaths cVerification
    , cScanners
    }

-- | Type alias for VerifyConfig' with all required fields.
type VerifyConfig = VerifyConfig' Identity

-- | Config of verification.
data VerifyConfig' f = VerifyConfig
  { vcAnchorSimilarityThreshold :: Field f Double
  , vcExternalRefCheckTimeout   :: Field f (Time Second)
  , vcVirtualFiles              :: Field f [RelGlobPattern]
    -- ^ Files which we pretend do exist.
  , vcNotScanned                :: Field f [RelGlobPattern]
    -- ^ Files, references in which we should not analyze.
  , vcIgnoreRefs                :: Field f [Regex]
    -- ^ Regular expressions that match external references we should not verify.
  , vcIgnoreAuthFailures        :: Field f Bool
    -- ^ If True - links which return 403 or 401 code will be skipped,
    -- otherwise – will be marked as broken, because we can't check it.
  , vcDefaultRetryAfter         :: Field f (Time Second)
    -- ^ Default Retry-After delay, applicable when we receive a 429 response
    -- and it does not contain a @Retry-After@ header.
  , vcMaxRetries                :: Field f Int
  } deriving stock (Generic)

normaliseVerifyConfigFilePaths :: VerifyConfig -> VerifyConfig
normaliseVerifyConfigFilePaths vc@VerifyConfig{ vcVirtualFiles, vcNotScanned}
  = vc
    { vcVirtualFiles = map normaliseGlobPattern vcVirtualFiles
    , vcNotScanned = map normaliseGlobPattern vcNotScanned
    }

-- | Configs for all the supported scanners.
data ScannersConfig = ScannersConfig
  { scMarkdown :: MarkdownConfig
  } deriving stock (Generic)

makeLensesWith postfixFields ''Config'
makeLensesWith postfixFields ''VerifyConfig'

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
                "Key " <> showBs key <> " requires replacement with an item, \
                \but list was given"

      | Just [_wholeMatch, leadingChars, key] <-
          R.getAllTextSubmatches <$> (holeListRegex `R.matchM` holeLine) ->
            case getReplacement key of
              Left _ -> error $
                "Key " <> showBs key <> " requires replacement with a list, \
                \but an item was given"
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

    , "notScanned" -: Right $ case flavor of
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

    , "virtualFiles" -: Right $ case flavor of
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
    { cTraversal = TraversalConfig ignored
    , cVerification = maybe defVerification overrideVerify $ cVerification config
    , cScanners = cScanners config
    }
  where
    flavor = mcFlavor . scMarkdown $ cScanners config

    defTraversal = cTraversal $ defConfig flavor

    ignored = fromMaybe (tcIgnored defTraversal) $ tcIgnored =<< cTraversal config

    defVerification = cVerification $ defConfig flavor

    overrideVerify verifyConfig
      = VerifyConfig
        { vcAnchorSimilarityThreshold = overrideField vcAnchorSimilarityThreshold
        , vcExternalRefCheckTimeout   = overrideField vcExternalRefCheckTimeout
        , vcVirtualFiles              = overrideField vcVirtualFiles
        , vcNotScanned                = overrideField vcNotScanned
        , vcIgnoreRefs                = overrideField vcIgnoreRefs
        , vcIgnoreAuthFailures        = overrideField vcIgnoreAuthFailures
        , vcDefaultRetryAfter         = overrideField vcDefaultRetryAfter
        , vcMaxRetries                = overrideField vcMaxRetries
        }
      where
        overrideField :: (forall f. VerifyConfig' f -> Field f a) -> a
        overrideField field = fromMaybe (field defVerification) $ field verifyConfig

-----------------------------------------------------------
-- Yaml instances
-----------------------------------------------------------

instance KnownRatName unit => FromJSON (Time unit) where
  parseJSON = withText "time" $
    maybe (fail "Unknown time") pure . unitsP . toString

instance FromJSON Regex where
  parseJSON = withText "regex" $ \val -> do
    let errOrRegex = R.compile defaultCompOption defaultExecOption val
    either (error . show) return errOrRegex

-- Default boolean values according to
-- https://hackage.haskell.org/package/regex-tdfa-1.3.1.0/docs/Text-Regex-TDFA.html#t:CompOption
defaultCompOption :: CompOption
defaultCompOption = CompOption
  { caseSensitive = True
  , multiline = True
  , rightAssoc = True
  , newSyntax = True
  , lastStarGreedy = False
  }

-- ExecOption value to improve speed
defaultExecOption :: ExecOption
defaultExecOption = ExecOption {captureGroups = False}

instance FromJSON (ConfigOptional) where
  parseJSON = genericParseJSON aesonConfigOption

instance FromJSON (Config) where
  parseJSON = genericParseJSON aesonConfigOption

instance FromJSON (VerifyConfig' Maybe) where
  parseJSON = genericParseJSON aesonConfigOption

instance FromJSON (VerifyConfig) where
  parseJSON = genericParseJSON aesonConfigOption

instance FromJSON (ScannersConfig) where
  parseJSON = genericParseJSON aesonConfigOption
