{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Xrefcheck.System
  ( readingSystem
  , askWithinCI
  , RelGlobPattern (..)
  , bindGlobPattern
  ) where

import Universum

import Data.Aeson (FromJSON (..), withText)
import Data.Char qualified as C
import GHC.IO.Unsafe (unsafePerformIO)
import System.Directory (canonicalizePath)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.FilePath.Glob qualified as Glob

-- | We can quite safely treat surrounding filesystem as frozen,
-- so IO reading operations can be turned into pure values.
readingSystem :: IO a -> a
readingSystem = unsafePerformIO

-- | Heuristics to check whether we are running within CI.
-- Check the respective env variable which is usually set in all CIs.
askWithinCI :: IO Bool
askWithinCI = lookupEnv "CI" <&> \case
  Just "1"                       -> True
  Just (map C.toLower -> "true") -> True
  _                              -> False

-- | Glob pattern relative to repository root.
newtype RelGlobPattern = RelGlobPattern FilePath

bindGlobPattern :: FilePath -> RelGlobPattern -> Glob.Pattern
bindGlobPattern root (RelGlobPattern relPat) = readingSystem $ do
  -- TODO [#26] try to avoid using canonicalization
  absPat <- canonicalizePath (root </> relPat)
  case Glob.tryCompileWith globCompileOptions absPat of
    Left err -> error $
      "Glob pattern compilation failed after canonicalization: " <> toText err
    Right pat ->
      return pat

instance FromJSON RelGlobPattern where
  parseJSON = withText "Repo-relative glob pattern" $ \path -> do
    let spath = toString path
    -- Checking path is sane
    _ <- Glob.tryCompileWith globCompileOptions spath
      & either fail pure
    return (RelGlobPattern spath)

-- | Glob compilation options we use.
globCompileOptions :: Glob.CompOptions
globCompileOptions = Glob.compDefault
