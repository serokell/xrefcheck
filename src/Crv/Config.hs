{-# OPTIONS_GHC -fno-warn-orphans #-}

module Crv.Config where

import Data.Aeson (FromJSON (..), withText)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveFromJSON)
import Data.Default (Default (..))
import Time (KnownRatName, Second, Time, sec, unitsP)

import Crv.System (CanonicalizedGlobPattern)

-- | Overall config.
data Config = Config
    { cTraversal    :: TraversalConfig
    , cVerification :: VerifyConfig
    }

-- | Config of repositry traversal.
data TraversalConfig = TraversalConfig
    { tcIgnored   :: [FilePath]
      -- ^ Folders, files in which we completely ignore.
    }

-- | Config of verification.
data VerifyConfig = VerifyConfig
    { vcAnchorSimilarityThreshold :: Double
    , vcExternalRefCheckTimeout   :: Time Second
    , vcVirtualFiles              :: [CanonicalizedGlobPattern]
      -- ^ Files which we pretend do exist.
    , vcNotScanned                :: [FilePath]
      -- ^ Folders, references in files of which we should not analyze.
    }

-----------------------------------------------------------
-- Default instances
-----------------------------------------------------------

instance Default Config where
    def =
        Config
        { cTraversal = def
        , cVerification = def
        }

instance Default TraversalConfig where
    def = TraversalConfig []

instance Default VerifyConfig where
    def =
        VerifyConfig
        { vcAnchorSimilarityThreshold = 0.5
        , vcExternalRefCheckTimeout = sec 3
        , vcVirtualFiles = []
        , vcNotScanned = []
        }

-----------------------------------------------------------
-- Yaml instances
-----------------------------------------------------------

deriveFromJSON defaultOptions ''Config
deriveFromJSON defaultOptions ''TraversalConfig
deriveFromJSON defaultOptions ''VerifyConfig

instance KnownRatName unit => FromJSON (Time unit) where
    parseJSON = withText "time" $
        maybe (fail "Unknown time") pure . unitsP . toString
