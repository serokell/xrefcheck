{- SPDX-FileCopyrightText: 2018-2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

-- | Generalised repo scanner and analyser.

module Xrefcheck.Scan
  ( TraversalConfig
  , TraversalConfig' (..)
  , Extension
  , ScanAction
  , FormatsSupport
  , RepoInfo (..)
  , ScanError (..)
  , ScanResult (..)

  , normaliseTraversalConfigFilePaths
  , scanRepo
  , specificFormatsSupport
  ) where

import Universum

import Data.Aeson (FromJSON (..), genericParseJSON)
import Data.Foldable qualified as F
import Data.Map qualified as M
import Data.Reflection (Given)
import Fmt (Buildable (..), nameF, (+|), (|+))
import System.Console.Pretty (Style (..))
import System.Directory (doesDirectoryExist)
import System.Directory.Tree qualified as Tree
import System.FilePath (dropTrailingPathSeparator, equalFilePath, takeDirectory, takeExtension)

import Xrefcheck.Core
import Xrefcheck.Progress
import Xrefcheck.System (RelGlobPattern, matchesGlobPatterns, normaliseGlobPattern, readingSystem)
import Xrefcheck.Util (ColorMode, Field, aesonConfigOption, normaliseWithNoTrailing, styleIfNeeded)

-- | Type alias for TraversalConfig' with all required fields.
type TraversalConfig = TraversalConfig' Identity

-- | Config of repositry traversal.
data TraversalConfig' f = TraversalConfig
  { tcIgnored :: Field f [RelGlobPattern]
    -- ^ Files and folders, files in which we completely ignore.
  } deriving stock (Generic)

instance FromJSON (TraversalConfig' Maybe) where
  parseJSON = genericParseJSON aesonConfigOption

instance FromJSON (TraversalConfig) where
  parseJSON = genericParseJSON aesonConfigOption

normaliseTraversalConfigFilePaths :: TraversalConfig -> TraversalConfig
normaliseTraversalConfigFilePaths = TraversalConfig . map normaliseGlobPattern . tcIgnored

-- | File extension, dot included.
type Extension = String

-- | Way to parse a file.
type ScanAction = FilePath -> IO (FileInfo, [ScanError])

-- | All supported ways to parse a file.
type FormatsSupport = Extension -> Maybe ScanAction

data ScanResult = ScanResult
  { srScanErrors :: [ScanError]
  , srRepoInfo   :: RepoInfo
  } deriving stock (Show)

data ScanError = ScanError
  { sePosition    :: Position
  , seFile        :: FilePath
  , seDescription :: Text
  } deriving stock (Show, Eq)

instance Given ColorMode => Buildable ScanError where
  build ScanError{..} =
    "In file " +| styleIfNeeded Faint (styleIfNeeded Bold seFile) |+ "\n"
    +| nameF ("scan error " +| sePosition |+ "") mempty |+ "\n⛀  "
    +| seDescription |+ "\n\n\n"

specificFormatsSupport :: [([Extension], ScanAction)] -> FormatsSupport
specificFormatsSupport formats = \ext -> M.lookup ext formatsMap
  where
    formatsMap = M.fromList
        [ (extension, parser)
        | (extensions, parser) <- formats
        , extension <- extensions
        ]

scanRepo
  :: MonadIO m
  => Rewrite -> FormatsSupport -> TraversalConfig -> FilePath -> m ScanResult
scanRepo rw formatsSupport config root = do
  putTextRewrite rw "Scanning repository..."

  when (not $ isDirectory root) $
    die $ "Repository's root does not seem to be a directory: " <> root

  _ Tree.:/ repoTree <- liftIO $ Tree.readDirectoryWithL processFile root
  let (errs, fileInfos) = gatherScanErrs &&& gatherFileInfos
        $ dropSndMaybes . F.toList
        $ Tree.zipPaths $ location Tree.:/ repoTree
  return . ScanResult errs $ RepoInfo (M.fromList fileInfos)
  where
    isDirectory = readingSystem . doesDirectoryExist
    gatherScanErrs = foldMap (snd . snd)
    gatherFileInfos = map (bimap normaliseWithNoTrailing fst)

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
