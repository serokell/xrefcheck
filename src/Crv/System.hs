module Crv.System
    ( readingSystem
    , CanonicalizedGlobPattern (..)
    ) where

import Data.Aeson (FromJSON (..), withText)
import GHC.IO.Unsafe (unsafePerformIO)
import System.Directory (canonicalizePath)
import qualified System.FilePath.Glob as Glob

-- | We can quite safely treat surrounding filesystem as frozen,
-- so IO reading operations can be turned into pure values.
readingSystem :: IO a -> a
readingSystem = unsafePerformIO

-- | Glob pattern with 'canonicalizePath' applied O_o.
newtype CanonicalizedGlobPattern = CanonicalizedGlobPattern Glob.Pattern

instance FromJSON CanonicalizedGlobPattern where
    parseJSON = withText "Repo-rooted glob pattern" $ \path -> do
        let !cpath = readingSystem $ canonicalizePath (toString path)
        cpat <- Glob.tryCompileWith Glob.compDefault cpath
                & either fail pure
        return $ CanonicalizedGlobPattern cpat
