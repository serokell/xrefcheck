{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Xrefcheck.System
  ( readingSystem
  , askWithinCI

  , CanonicalPath
  , canonicalizePath
  , unCanonicalPath
  , getDirsBetweenRootAndFile
  , getPosixRelativeChild
  , getPosixRelativeOrAbsoluteChild
  , hasIndirectionThroughParent
  , takeDirectory
  , takeExtension
  , (</)

  , RelGlobPattern (unRelGlobPattern)
  , mkGlobPattern
  , bindGlobPattern
  , matchesGlobPatterns
  ) where

import Universum

import Data.Aeson (FromJSON (..), withText)
import Data.Char qualified as C
import Data.List (stripPrefix)
import GHC.IO.Unsafe (unsafePerformIO)
import System.Directory qualified as Directory
import System.Environment (lookupEnv)
import System.FilePath qualified as FP
import System.FilePath.Glob qualified as Glob
import System.FilePath.Posix qualified as FPP
import Text.Interpolation.Nyan (int, rmode')

-- | We can quite safely treat surrounding filesystem as frozen,
-- so IO reading operations can be turned into pure values.
readingSystem :: IO a -> a
readingSystem = unsafePerformIO

-- | Heuristics to check whether we are running within CI.
-- Check the respective env variable which is usually set in all CIs.
askWithinCI :: IO Bool
askWithinCI = lookupEnv "CI" <&> \case
  Just "1" -> True
  Just (map C.toLower -> "true") -> True
  _ -> False

-- | A FilePath that has been canonicalized.
--
-- It should be created via 'canonicalizePath'.
--
-- Currently, canonical paths have been made absolute, normalised
-- regarding the running platform (e.g. Posix or Windows), with
-- indirections syntactically expanded as much as possible and
-- with no trailing path separator. All this results in a weaker
-- version than that provided by 'System.Directory.canonicalizePath'.
newtype CanonicalPath = UnsafeCanonicalPath
  { unCanonicalPath :: FilePath
  } deriving newtype (Show, Eq, Ord)

canonicalizePath :: FilePath -> IO CanonicalPath
canonicalizePath = fmap canonicalize . Directory.makeAbsolute
  where
    canonicalize :: FilePath -> CanonicalPath
    canonicalize = UnsafeCanonicalPath
      . expandIndirections
      . FP.normalise
      . FP.dropTrailingPathSeparator

    expandIndirections :: FilePath -> FilePath
    expandIndirections = FP.joinPath
      . reverse
      . expand 0
      . reverse
      . FP.splitDirectories

    expand :: Int -> [FilePath] -> [FilePath]
    expand acc (".." : xs) = expand (acc + 1) xs
    expand acc ("." : xs) = expand acc xs
    expand 0 (x : xs) = x : expand 0 xs
    expand acc (_ : xs) = expand (acc - 1) xs
    expand acc [] = replicate acc ".."

-- | 'FilePath.takeDirectory' version for 'CanonicalPath'.
takeDirectory :: CanonicalPath -> CanonicalPath
takeDirectory (UnsafeCanonicalPath p) = UnsafeCanonicalPath $ FP.takeDirectory p

-- | 'FilePath.takeExtension' version for 'CanonicalPath'.
takeExtension :: CanonicalPath -> String
takeExtension (UnsafeCanonicalPath p) = FP.takeExtension p

-- | Get the list of directories, canonicalized, between two given paths.
getDirsBetweenRootAndFile :: CanonicalPath -> CanonicalPath -> [CanonicalPath]
getDirsBetweenRootAndFile (UnsafeCanonicalPath rootPath) file =
  case stripPrefix rootPath (unCanonicalPath (takeDirectory file)) of
    Just path -> UnsafeCanonicalPath <$> scanl (FP.</>) rootPath directories
      where
        directories = FP.splitDirectories $ dropWhile FP.isPathSeparator path
    Nothing -> []

-- | Get a relative 'FilePath' from the second given path (child) with
-- respect to the first one (root).
--
-- It returns Nothing if child cannot be reached from root downwards
-- in the filesystem tree.
--
-- The resulting `FilePath` uses POSIX path separators.
getPosixRelativeChild :: CanonicalPath -> CanonicalPath -> Maybe FilePath
getPosixRelativeChild (UnsafeCanonicalPath root) (UnsafeCanonicalPath child) =
  dropLeadingSepAndEmptyCase . fmap replaceSeparator <$> stripPrefix root child
  where
    replaceSeparator :: Char -> Char
    replaceSeparator c
      | FP.isPathSeparator c = FPP.pathSeparator
      | otherwise = c

    dropLeadingSepAndEmptyCase :: FilePath -> FilePath
    dropLeadingSepAndEmptyCase path = case dropWhile FP.isPathSeparator path of
      "" -> "."
      other -> other

-- | Get the relative 'FilePath' using 'getPosixRelativeChild', but
-- return the same passed absolute path instead of 'Nothing'.
getPosixRelativeOrAbsoluteChild :: CanonicalPath -> CanonicalPath -> FilePath
getPosixRelativeOrAbsoluteChild root child =
  fromMaybe (unCanonicalPath child) (getPosixRelativeChild root child)

-- | Check if some 'FilePath' passes through its parent while
-- expanding indirections.
hasIndirectionThroughParent :: FilePath -> Bool
hasIndirectionThroughParent = go 0 . FP.splitDirectories
  where
    go :: Int -> [FilePath] -> Bool
    go _ [] = False
    go 0 (".." : _) = True
    go acc (".." : xs) = go (acc - 1) xs
    go acc ("." : xs) = go acc xs
    go acc (_ : xs) = go (acc + 1) xs

-- | Extend some 'CanonicalPath' with a given relative 'FilePath'.
--
-- The right-hand side 'FilePath' can use both Posix and Windows
-- path separators.
(</) :: CanonicalPath -> FilePath -> IO CanonicalPath
UnsafeCanonicalPath p </ f = canonicalizePath $ p FP.</> f
infixr 5 </

-- | Glob pattern relative to repository root.
--
-- It should be created via 'mkGlobPattern'.
newtype RelGlobPattern = UnsafeRelGlobPattern
  { unRelGlobPattern :: FilePath
  }

mkGlobPattern :: ToString s => s -> Either String RelGlobPattern
mkGlobPattern path = do
  let spath = toString path
  unless (FPP.isRelative spath) $ Left $
    "Expected a relative glob pattern, but got " <> spath
  -- Checking correctness of glob, e.g. "a[b" is incorrect
  case Glob.tryCompileWith globCompileOptions spath of
    Right _ -> return (UnsafeRelGlobPattern spath)
    Left err -> Left
        [int||
        Glob pattern compilation failed.
        Error message is:
        #{err}
        The syntax for glob patterns is described here:
        https://hackage.haskell.org/package/Glob/docs/System-FilePath-Glob.html#v:compile
        Special characters in file names can be escaped using square brackets, e.g. <a> -> [<]a[>].
        |]

bindGlobPattern :: CanonicalPath -> RelGlobPattern -> Glob.Pattern
bindGlobPattern root (UnsafeRelGlobPattern relPat) = readingSystem $ do
  UnsafeCanonicalPath absPat <- root </ relPat
  case Glob.tryCompileWith globCompileOptions absPat of
    Left err -> error $
      "Glob pattern compilation failed after canonicalization: " <> toText err
    Right pat ->
      return pat

matchesGlobPatterns :: CanonicalPath -> [RelGlobPattern] -> CanonicalPath -> Bool
matchesGlobPatterns root globPatterns file = or
  [ Glob.match pat $ unCanonicalPath file
  | globPattern <- globPatterns
  , let pat = bindGlobPattern root globPattern
  ]

instance FromJSON RelGlobPattern where
  parseJSON = withText "Repo-relative glob pattern" $
    either fail pure . mkGlobPattern

-- | Glob compilation options we use.
globCompileOptions :: Glob.CompOptions
globCompileOptions = Glob.compDefault{Glob.errorRecovery = False}
