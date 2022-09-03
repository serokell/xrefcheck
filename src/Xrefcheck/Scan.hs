{- SPDX-FileCopyrightText: 2018-2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

-- | Generalised repo scanner and analyser.

module Xrefcheck.Scan
  ( TraversalConfig (..)
  , Extension
  , ScanAction
  , FormatsSupport
  , RepoInfo (..)

  , normaliseTraversalConfigFilePaths
  , gatherRepoInfo
  , specificFormatsSupport
  ) where

import Universum

import Data.Aeson.TH (deriveFromJSON)
import Data.Foldable qualified as F
import Data.Map qualified as M
import System.Directory (doesDirectoryExist)
import System.Directory.Tree qualified as Tree
import System.FilePath (dropTrailingPathSeparator, takeDirectory, takeExtension, equalFilePath)

import Xrefcheck.Core
import Xrefcheck.Progress
import Xrefcheck.System (readingSystem, RelGlobPattern, normaliseGlobPattern, matchesGlobPatterns)
import Xrefcheck.Util (aesonConfigOption, normaliseWithNoTrailing)

-- | Config of repositry traversal.
data TraversalConfig = TraversalConfig
  { tcIgnored   :: [RelGlobPattern]
    -- ^ Files and folders, files in which we completely ignore.
  }

normaliseTraversalConfigFilePaths :: TraversalConfig -> TraversalConfig
normaliseTraversalConfigFilePaths = TraversalConfig . map normaliseGlobPattern . tcIgnored

deriveFromJSON aesonConfigOption ''TraversalConfig

-- | File extension, dot included.
type Extension = String

-- | Way to parse a file.
type ScanAction = FilePath -> IO FileInfo

-- | All supported ways to parse a file.
type FormatsSupport = Extension -> Maybe ScanAction

specificFormatsSupport :: [([Extension], ScanAction)] -> FormatsSupport
specificFormatsSupport formats = \ext -> M.lookup ext formatsMap
  where
    formatsMap = M.fromList
        [ (extension, parser)
        | (extensions, parser) <- formats
        , extension <- extensions
        ]

gatherRepoInfo
  :: MonadIO m
  => Rewrite -> FormatsSupport -> TraversalConfig -> FilePath -> m RepoInfo
gatherRepoInfo rw formatsSupport config root = do
  putTextRewrite rw "Scanning repository..."

  when (not $ isDirectory root) $
    die $ "Repository's root does not seem to be a directory: " <> root

  _ Tree.:/ repoTree <- liftIO $ Tree.readDirectoryWithL processFile root
  let fileInfos = map (first normaliseWithNoTrailing)
        $ dropSndMaybes . F.toList
        $ Tree.zipPaths $ location Tree.:/ repoTree
  return $ RepoInfo (M.fromList fileInfos)
  where
    isDirectory = readingSystem . doesDirectoryExist

    processFile file = do
      let ext = takeExtension file
      let mscanner = formatsSupport ext
      if isIgnored file
        then pure Nothing
        else forM mscanner ($ file)
    dropSndMaybes l = [(a, b) | (a, Just b) <- l]

    isIgnored = matchesGlobPatterns root $ tcIgnored config

    -- The context location of the root.
    -- This is done by removing the last component from the path.
    -- > root = "./folder/file.md"       ==> location = "./folder"
    -- > root = "./folder/subfolder"     ==> location = "./folder"
    -- > root = "./folder/subfolder/"    ==> location = "./folder"
    -- > root = "./folder/subfolder/./"  ==> location = "./folder/subfolder"
    -- > root = "."                      ==> location = ""
    -- > root = "/absolute/path"         ==> location = "/absolute"
    -- > root = "/"                      ==> location = "/"
    location =
      if root `equalFilePath` "."
        then ""
        else takeDirectory $ dropTrailingPathSeparator root
