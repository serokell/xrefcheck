module Crv.Config where

import           Data.Default (Default (..))

data Config = Config
    { cTraversal :: TraversalConfig
    }

instance Default Config where
    def = Config def

data TraversalConfig = TraversalConfig
    { tcExcluded :: [FilePath]
    }

instance Default TraversalConfig where
    def = TraversalConfig [".stack-work"]
