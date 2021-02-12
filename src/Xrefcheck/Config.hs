{- SPDX-FileCopyrightText: 2018-2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Xrefcheck.Config where

import Control.Lens (makeLensesWith)
import Data.Aeson.TH (deriveFromJSON)
import Data.Yaml (FromJSON (..), decodeEither', prettyPrintParseException, withText)
import Instances.TH.Lift ()
import Text.Regex.TDFA (CompOption (..), ExecOption (..), Regex)
import Text.Regex.TDFA.Text (compile)

-- FIXME: Use </> from System.FilePath
-- </> from Posix is used only because we cross-compile to Windows and \ doesn't work on Linux
import Data.FileEmbed (embedFile)
import System.FilePath.Posix ((</>))
import Time (KnownRatName, Second, Time, unitsP)

import Xrefcheck.Scan
import Xrefcheck.Scanners.Markdown
import Xrefcheck.System (RelGlobPattern)
import Xrefcheck.Util (aesonConfigOption, postfixFields)

-- | Overall config.
data Config = Config
    { cTraversal    :: TraversalConfig
    , cVerification :: VerifyConfig
    , cScanners     :: ScannersConfig
    }

-- | Config of verification.
data VerifyConfig = VerifyConfig
    { vcAnchorSimilarityThreshold :: Double
    , vcExternalRefCheckTimeout   :: Time Second
    , vcVirtualFiles              :: [RelGlobPattern]
      -- ^ Files which we pretend do exist.
    , vcNotScanned                :: [FilePath]
      -- ^ Prefixes of files, references in which we should not analyze.
    , vcIgnoreRefs                :: Maybe [Regex]
      -- ^ Regular expressions that match external references we should not verify.
    }

-- | Configs for all the supported scanners.
data ScannersConfig = ScannersConfig
    { scMarkdown :: MarkdownConfig
    }

makeLensesWith postfixFields ''Config
makeLensesWith postfixFields ''VerifyConfig

-----------------------------------------------------------
-- Default config
-----------------------------------------------------------

-- | Default config in textual representation.
--
-- Sometimes you cannot just use 'defConfig' because clarifying comments
-- would be lost.
defConfigText :: ByteString
defConfigText =
  $(embedFile ("src-files" </> "def-config.yaml"))

defConfig :: HasCallStack => Config
defConfig =
  either (error . toText . prettyPrintParseException) id $
  decodeEither' defConfigText

-----------------------------------------------------------
-- Yaml instances
-----------------------------------------------------------

deriveFromJSON aesonConfigOption ''Config
deriveFromJSON aesonConfigOption ''ScannersConfig
deriveFromJSON aesonConfigOption ''VerifyConfig

instance KnownRatName unit => FromJSON (Time unit) where
    parseJSON = withText "time" $
        maybe (fail "Unknown time") pure . unitsP . toString

instance FromJSON Regex where
    parseJSON = withText "regex" $ \val -> do
        let errOrRegex =
                compile defaultCompOption defaultExecOption val
        either (error . show) return errOrRegex

-- Default boolean values according to
-- https://hackage.haskell.org/package/regex-tdfa-1.3.1.0/docs/Text-Regex-TDFA.html#t:CompOption
defaultCompOption :: CompOption
defaultCompOption =
    CompOption
    { caseSensitive = True
    , multiline = True
    , rightAssoc = True
    , newSyntax = True
    , lastStarGreedy = False
    }

-- ExecOption value to improve speed
defaultExecOption :: ExecOption
defaultExecOption = ExecOption {captureGroups = False}
