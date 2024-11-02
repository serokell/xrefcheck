{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Xrefcheck.System
  ( askWithinCI

  , RelPosixLink (..)
  , (</>)
  , mkRelPosixLink
  , filePathFromRoot
  , getIntermediateDirs
  , hasBackslash
  , takeDirectory
  , takeExtension

  , CanonicalRelPosixLink (unCanonicalRelPosixLink)
  , hasUnexpanededParentIndirections
  , canonicalizeRelPosixLink

  , CanonicalRelGlobPattern (unCanonicalRelGlobPattern)
  , matchesGlobPatterns
  , mkCanonicalRelGlobPattern

  , PrintUnixPaths(..)
  , mkPathForPrinting
  ) where

import Universum

import Data.Aeson (FromJSON (..), withText)
import Data.Char qualified as C
import Data.Reflection (Given (..))
import Data.Text qualified as T
import Fmt (Buildable)
import System.Console.Pretty (Pretty)
import System.Environment (lookupEnv)
import System.FilePath qualified as FP
import System.FilePath.Glob qualified as Glob
import System.FilePath.Posix qualified as FPP
import Text.Interpolation.Nyan (int, rmode')

-- | Heuristics to check whether we are running within CI.
-- Check the respective env variable which is usually set in all CIs.
askWithinCI :: IO Bool
askWithinCI = lookupEnv "CI" <&> \case
  Just "1" -> True
  Just (map C.toLower -> "true") -> True
  _ -> False

-- | Relative file path with POSIX path separators.
--
-- This type exist in contrast to 'FilePath' which, in this project,
-- is used for platform-dependent file paths and related filesystem
-- IO operations.
--
-- Note that `RelPosixLink` may contain `\` characters, but they are
-- considered as part of the filename instead of denoting a path
-- separator.
newtype RelPosixLink = RelPosixLink
  { unRelPosixLink :: Text
  } deriving newtype (Show, Eq, Ord, NFData, Buildable, Pretty)

-- | Create a POSIX file path from a platform-dependent one.
mkRelPosixLink :: FilePath -> RelPosixLink
mkRelPosixLink = RelPosixLink
  . withPathSeparator FPP.pathSeparator
  . fromString

-- | Join two 'RelPosixLink's.
(</>) :: RelPosixLink -> RelPosixLink -> RelPosixLink
RelPosixLink a </> RelPosixLink b =
  let a' = fromMaybe a $ T.stripSuffix "/" a
      b' = fromMaybe b $ T.stripPrefix "./" a
  in case (a', b') of
        ("", _) -> RelPosixLink b
        (".", _) -> RelPosixLink b
        _ -> RelPosixLink $ a' <> "/" <> b'

-- Get the platform-dependent file path from a 'RelPosixLink'
-- considered as relative to another given platform-dependent
-- 'FilePath'.
--
-- In Windows, every `\` occurrence will be replaced by `/`.
filePathFromRoot :: FilePath -> RelPosixLink -> FilePath
filePathFromRoot rootPath = (rootPath FP.</>)
  . toString
  . withPathSeparator FP.pathSeparator
  . unRelPosixLink

-- | 'FilePath.takeDirectory' version for 'RelPosixLink'.
takeDirectory :: RelPosixLink -> RelPosixLink
takeDirectory = RelPosixLink
  . fromString
  . FPP.takeDirectory
  . toString
  . unRelPosixLink

-- | 'FilePath.takeExtension' version for 'RelPosixLink'.
takeExtension :: RelPosixLink -> String
takeExtension = FPP.takeExtension
  . toString
  . unRelPosixLink

-- | 'Check if a 'RelPosixLink' contains any backslash.
hasBackslash :: RelPosixLink -> Bool
hasBackslash = ('\\' `elem`)
  . unRelPosixLink

-- | Get the list of directories between a 'RelPosixLink' and its
-- relative root.
getIntermediateDirs :: RelPosixLink -> [RelPosixLink]
getIntermediateDirs link = fmap RelPosixLink $
  case T.splitOn "/" $ unRelPosixLink $ takeDirectory link of
    [] -> []
    ["."] -> [""]
    [".."] -> ["", ".."]
    d : ds -> scanl (\a b -> a <> "/" <> b) d ds

-- | Relative POSIX file path with some normalizations applied.
--
-- It should be created from a 'RelPosixLink' via
-- 'canonicalizeRelPosixLink'.
newtype CanonicalRelPosixLink = UnsafeCanonicalRelPosixLink
  { unCanonicalRelPosixLink :: RelPosixLink
  } deriving newtype (Show, Eq, Ord, NFData, Buildable, Pretty)

-- | Canonicalize a 'RelPosixLink'.
--
-- Applies the following normalizations:
--
--  * Drop trailing path separator.
--
--  * Expand '.' and '..' indirections syntactically.
--
canonicalizeRelPosixLink :: RelPosixLink -> CanonicalRelPosixLink
canonicalizeRelPosixLink = UnsafeCanonicalRelPosixLink
  . RelPosixLink
  . expandPosixIndirections
  . dropTrailingPosixPathSeparator
  . withPathSeparator FPP.pathSeparator
  . unRelPosixLink

-- | Check if a 'CanonicalRelPosixLink' passes through its relative root when
-- expanding indirections.
hasUnexpanededParentIndirections :: CanonicalRelPosixLink -> Bool
hasUnexpanededParentIndirections = elem ".."
  . T.splitOn "/"
  . unRelPosixLink
  . unCanonicalRelPosixLink

-- | Relative Glob pattern with some normalizations applied.
--
-- It should be created via 'mkCanonicalRelGlobPattern'.
newtype CanonicalRelGlobPattern = UnsafeCanonicalRelGlobPattern
  { unCanonicalRelGlobPattern :: Glob.Pattern
  }

-- | Create a CanonicalRelGlobPattern from a 'ToString' instance value that
-- represents a POSIX glob pattern.
--
-- Applies the following normalizations:
--
--  * Drop trailing path separator.
--
--  * FilePath.Posix.normalise.
--
--  * Expand '.' and '..' indirections syntactically.
--
mkCanonicalRelGlobPattern :: ToString s => s -> Either String CanonicalRelGlobPattern
mkCanonicalRelGlobPattern path = do
  let spath = toString path
  unless (FPP.isRelative spath) $ Left $
    "Expected a relative glob pattern, but got " <> spath
  -- Checking correctness of glob, e.g. "a[b" is incorrect
  case Glob.tryCompileWith globCompileOptions (normalise spath) of
    Right pat -> return $ UnsafeCanonicalRelGlobPattern pat
    Left err -> Left
        [int||
        Glob pattern compilation failed.
        Error message is:
        #{err}
        The syntax for glob patterns is described here:
        https://hackage.haskell.org/package/Glob/docs/System-FilePath-Glob.html#v:compile
        Special characters in file names can be escaped using square brackets, e.g. <a> -> [<]a[>].
        |]
  where
    normalise = toString
      . expandPosixIndirections
      . fromString
      . FPP.normalise
      . FPP.dropTrailingPathSeparator

-- Checks if a 'CanonicalRelPosixLink' matches some of the given
-- 'CanonicalRelGlobPattern's.
--
-- They are considered as relative to the same root.
matchesGlobPatterns :: [CanonicalRelGlobPattern] -> CanonicalRelPosixLink -> Bool
matchesGlobPatterns globPatterns file = or
  [ Glob.match pat . toString . unRelPosixLink . unCanonicalRelPosixLink $ file
  | UnsafeCanonicalRelGlobPattern pat <- globPatterns
  ]

instance FromJSON CanonicalRelGlobPattern where
  parseJSON = withText "Repo-relative glob pattern" $
    either fail pure . mkCanonicalRelGlobPattern

-- | Glob compilation options we use.
globCompileOptions :: Glob.CompOptions
globCompileOptions = Glob.compDefault{Glob.errorRecovery = False}

dropTrailingPosixPathSeparator :: Text -> Text
dropTrailingPosixPathSeparator p = fromMaybe p
  $ T.stripSuffix "/" p

-- Expand '.' and '..' in paths with Posix path separators.
expandPosixIndirections :: Text -> Text
expandPosixIndirections = T.intercalate "/"
    . reverse
    . expand 0
    . reverse
    . T.split (FPP.isPathSeparator)
  where
    expand :: Int -> [Text] -> [Text]
    expand acc (".." : xs) = expand (acc + 1) xs
    expand acc ("." : xs) = expand acc xs
    expand 0 (x : xs) = x : expand 0 xs
    expand acc (_ : xs) = expand (acc - 1) xs
    expand acc [] = replicate acc ".."

-- Expand '.' and '..' in paths with system-specific path separators.
expandPathIndirections :: FilePath -> FilePath
expandPathIndirections = FP.joinPath
    . reverse
    . expand 0
    . reverse
    . map FP.dropTrailingPathSeparator
    . FP.splitPath
  where
    expand :: Int -> [FilePath] -> [FilePath]
    expand acc (".." : xs) = expand (acc + 1) xs
    expand acc ("." : xs) = expand acc xs
    expand 0 (x : xs) = x : expand 0 xs
    expand acc (_ : xs) = expand (acc - 1) xs
    expand acc [] = replicate acc ".."

withPathSeparator :: Char -> Text -> Text
withPathSeparator pathSep = T.map replaceSeparator
  where
    replaceSeparator :: Char -> Char
    replaceSeparator c
      | FP.isPathSeparator c = pathSep
      | otherwise = c

newtype PrintUnixPaths = PrintUnixPaths Bool

mkPathForPrinting :: Given PrintUnixPaths => FilePath -> String
mkPathForPrinting = replaceSeparator . expandPathIndirections
  where
    replaceSeparator :: FilePath -> String
    replaceSeparator = case given of
      PrintUnixPaths True  -> map (\c -> if c == FP.pathSeparator then '/' else c)
      PrintUnixPaths False -> id
