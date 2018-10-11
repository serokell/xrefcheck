{-# OPTIONS_GHC -fno-warn-orphans #-}

module Crv.Config where

import Data.Aeson (FromJSON (..), withText)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveFromJSON)
import Data.Default (Default (..))
import Time (KnownRatName, Second, Time, sec, unitsP)

-- | Overall config.
data Config = Config
    { cTraversal    :: TraversalConfig
    , cVerification :: VerifyConfig
    , cVerbose      :: Bool
    }

-- | Config of repositry traversal.
data TraversalConfig = TraversalConfig
    { tcExcluded :: [FilePath]
    }

-- | Config of verification.
data VerifyConfig = VerifyConfig
    { vcAnchorSimilarityThreshold :: Double
    , vcExternalRefCheckTimeout   :: Time Second
    }

-----------------------------------------------------------
-- Default instances
-----------------------------------------------------------

instance Default Config where
    def =
        Config
        { cTraversal = def
        , cVerification = def
        , cVerbose = False
        }

instance Default TraversalConfig where
    def = TraversalConfig []

instance Default VerifyConfig where
    def =
        VerifyConfig
        { vcAnchorSimilarityThreshold = 0.5
        , vcExternalRefCheckTimeout = sec 1
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
