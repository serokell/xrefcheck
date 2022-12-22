{- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE GADTs #-}

module Xrefcheck.RepoInfo
  ( RepoInfo
  , mkRepoInfo
  , riFiles
  , lookupFile
  , lookupDirectory
  ) where

import Universum

import Data.Char qualified as C
import Data.Map qualified as M
import Data.Reflection (Given)
import Fmt (Buildable (build), Builder)
import Text.Interpolation.Nyan

import Xrefcheck.Core
import Xrefcheck.Util

-- | Supose that we already have a type, `CanonicalPath`
-- that corresponds to a canonicalized `FilePath` (#197).
-- This is an example with an alias, and that is why
-- Golden tests are failing.
type CanonicalPath = FilePath

-- | The repository info: files and directories.
data RepoInfo = forall a. RepoInfo (RepoInfo' a)

-- | Generate a 'RepoInfo' with efficient path lookup depending
-- on the case-sensitivity of a given Markdown flavor.
mkRepoInfo
  :: Flavor
  -> [(CanonicalPath, FileStatus)]
  -> [(CanonicalPath, DirectoryStatus)] -> RepoInfo
mkRepoInfo flavor files directories =
    if caseInsensitiveAnchors flavor
    then RepoInfo $ RICaseInsensitive $ RepoInfoData
      { ridFiles = M.fromList $ fmap (first CaseInsensitivePath) $ files
      , ridDirectories = M.fromList $ fmap (first CaseInsensitivePath) $ directories
      }
    else RepoInfo $ RICaseSensitive $ RepoInfoData
      { ridFiles = M.fromList $ fmap (first CaseSensitivePath) $ files
      , ridDirectories = M.fromList $ fmap (first CaseSensitivePath) $ directories
      }

-- | All tracked files and directories.
data RepoInfoData a = RepoInfoData
  { ridFiles :: Map a FileStatus
    -- ^ Files from the repo with `FileInfo` attached to files that we've scanned.
  , ridDirectories :: Map a DirectoryStatus
    -- ^ Directories containing those files.
  }

data RepoInfo' a where
  RICaseInsensitive :: RepoInfoData CaseInsensitivePath -> RepoInfo' CaseInsensitivePath
  RICaseSensitive :: RepoInfoData CaseSensitivePath -> RepoInfo' CaseSensitivePath

-- Files from the repo with `FileInfo` attached to files that we've scanned.
riFiles :: RepoInfo -> [(CanonicalPath, FileStatus)]
riFiles (RepoInfo (RICaseInsensitive (RepoInfoData{..}))) =
  first unCaseInsensitivePath <$> toPairs ridFiles
riFiles (RepoInfo (RICaseSensitive (RepoInfoData{..}))) =
  first unCaseSensitivePath <$> toPairs ridFiles

-- Search for a file in the repository.
lookupFile :: CanonicalPath -> RepoInfo -> Maybe FileStatus
lookupFile path (RepoInfo (RICaseInsensitive (RepoInfoData{..}))) =
  M.lookup (CaseInsensitivePath path) ridFiles
lookupFile path (RepoInfo (RICaseSensitive (RepoInfoData{..}))) =
  M.lookup (CaseSensitivePath path) ridFiles

-- Search for a directory in the repository.
lookupDirectory :: CanonicalPath -> RepoInfo -> Maybe DirectoryStatus
lookupDirectory path (RepoInfo (RICaseInsensitive (RepoInfoData{..}))) =
  M.lookup (CaseInsensitivePath path) ridDirectories
lookupDirectory path (RepoInfo (RICaseSensitive (RepoInfoData{..}))) =
  M.lookup (CaseSensitivePath path) ridDirectories

data CaseSensitivePath = CaseSensitivePath
  { unCaseSensitivePath :: CanonicalPath
  } deriving stock (Show, Eq, Ord)

data CaseInsensitivePath = CaseInsensitivePath
  { unCaseInsensitivePath :: CanonicalPath
  } deriving stock (Show)

instance Eq CaseInsensitivePath where
  (CaseInsensitivePath p1) == (CaseInsensitivePath p2) =
    on (==) (fmap C.toLower) p1 p2

instance Ord CaseInsensitivePath where
  compare (CaseInsensitivePath p1) (CaseInsensitivePath p2) =
    on compare (fmap C.toLower) p1 p2

instance Given ColorMode => Buildable RepoInfo where
  build repoInfo
    | Just scanned <- nonEmpty [(name, info) | (name, Scanned info) <- riFiles repoInfo]
    = interpolateUnlinesF $ buildFileReport <$> scanned
    where
      buildFileReport :: (CanonicalPath, FileInfo) -> Builder
      buildFileReport (name, info) =
        [int||
        #{ colorIfNeeded Cyan $ name }:
        #{ interpolateIndentF 2 $ build info }
        |]
  build _ = "No scannable files found."
