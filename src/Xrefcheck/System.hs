{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Xrefcheck.System
  ( readingSystem
  , askWithinCI
  , RelGlobPattern
  , mkGlobPattern
  , normaliseGlobPattern
  , bindGlobPattern
  , matchesGlobPatterns
  ) where

import Universum

import Data.Aeson (FromJSON (..), withText)
import Data.Char qualified as C
import Data.Coerce (coerce)
import GHC.IO.Unsafe (unsafePerformIO)
import System.Directory (canonicalizePath)
import System.Environment (lookupEnv)
import System.FilePath.Glob qualified as Glob
import Text.Interpolation.Nyan
import System.FilePath.Posix (isRelative, (</>))

import Xrefcheck.Util (normaliseWithNoTrailing)

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

-- | Glob pattern relative to repository root. Should be created via @mkGlobPattern@
newtype RelGlobPattern = RelGlobPattern FilePath

mkGlobPattern :: ToString s => s -> Either String RelGlobPattern
mkGlobPattern path = do
  let spath = toString path
  unless (isRelative spath) $ Left $
    "Expected a relative glob pattern, but got " <> spath
  -- Checking correctness of glob, e.g. "a[b" is incorrect
  case Glob.tryCompileWith globCompileOptions spath of
    Right _ -> return (RelGlobPattern spath)
    Left err -> Left
        [int||
        Glob pattern compilation failed.
        Error message is:
        #{err}
        The syntax for glob patterns is described here:
        https://hackage.haskell.org/package/Glob/docs/System-FilePath-Glob.html#v:compile
        Special characters in file names can be escaped using square brackets, e.g. <a> -> [<]a[>].
        |]

normaliseGlobPattern :: RelGlobPattern -> RelGlobPattern
normaliseGlobPattern = RelGlobPattern . normaliseWithNoTrailing . coerce

bindGlobPattern :: FilePath -> RelGlobPattern -> Glob.Pattern
bindGlobPattern root (RelGlobPattern relPat) = readingSystem $ do
  -- TODO [#26] try to avoid using canonicalization
  absPat <- canonicalizePath (root </> relPat)
  case Glob.tryCompileWith globCompileOptions absPat of
    Left err -> error $
      "Glob pattern compilation failed after canonicalization: " <> toText err
    Right pat ->
      return pat

matchesGlobPatterns :: FilePath -> [RelGlobPattern] -> FilePath -> Bool
matchesGlobPatterns root globPatterns file = or
  [ Glob.match pat cFile
  | globPattern <- globPatterns
  , let pat = bindGlobPattern root globPattern
  , let cFile = readingSystem $ canonicalizePath file
  ]

instance FromJSON RelGlobPattern where
  parseJSON = withText "Repo-relative glob pattern" $
    either fail pure . mkGlobPattern

-- | Glob compilation options we use.
globCompileOptions :: Glob.CompOptions
globCompileOptions = Glob.compDefault{Glob.errorRecovery = False}
