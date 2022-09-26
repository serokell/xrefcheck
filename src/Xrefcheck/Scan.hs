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
  , ScanErrorDescription (..)
  , ScanResult (..)

  , normaliseTraversalConfigFilePaths
  , scanRepo
  , specificFormatsSupport
  ) where

import Universum

import Data.Aeson (FromJSON (..), genericParseJSON)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Reflection (Given)
import Fmt (Buildable (..), nameF, (+|), (|+))
import System.Directory (doesDirectoryExist)
import System.FilePath
  (dropTrailingPathSeparator, equalFilePath, splitDirectories, takeDirectory, takeExtension, (</>))
import System.Process (cwd, readCreateProcess, shell)

import Xrefcheck.Core
import Xrefcheck.Progress
import Xrefcheck.System (RelGlobPattern, matchesGlobPatterns, normaliseGlobPattern, readingSystem)
import Xrefcheck.Util

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
  , seDescription :: ScanErrorDescription
  } deriving stock (Show, Eq)

instance Given ColorMode => Buildable ScanError where
  build ScanError{..} =
    "In file " +| styleIfNeeded Faint (styleIfNeeded Bold seFile) |+ "\n"
    +| nameF ("scan error " +| sePosition |+ "") mempty |+ "\n⛀  "
    +| seDescription |+ "\n\n\n"

data ScanErrorDescription
  = LinkErr
  | FileErr
  | ParagraphErr Text
  | UnrecognisedErr Text
  deriving stock (Show, Eq)

instance Buildable ScanErrorDescription where
  build = \case
    LinkErr -> "Expected a LINK after \"ignore link\" annotation"
    FileErr -> "Annotation \"ignore file\" must be at the top of \
      \markdown or right after comments at the top"
    ParagraphErr txt -> "Expected a PARAGRAPH after \
          \\"ignore paragraph\" annotation, but found " +| txt |+ ""
    UnrecognisedErr txt ->  "Unrecognised option \"" +| txt |+ "\" perhaps you meant \
          \<\"ignore link\"|\"ignore paragraph\"|\"ignore file\"> "

specificFormatsSupport :: [([Extension], ScanAction)] -> FormatsSupport
specificFormatsSupport formats = \ext -> M.lookup ext formatsMap
  where
    formatsMap = M.fromList
        [ (extension, parser)
        | (extensions, parser) <- formats
        , extension <- extensions
        ]

-- | Process files that are tracked by git and not ignored by the config.
readDirectoryWith
  :: forall a. TraversalConfig
  -> (FilePath -> IO a)
  -> FilePath
  -> IO [(FilePath, a)]
readDirectoryWith config scanner root =
  traverse scanFile
  . filter (not . isIgnored)
  . fmap (location </>)
  . L.lines =<< readCreateProcess (shell "git ls-files"){cwd = Just root} ""
  where
    scanFile :: FilePath -> IO (FilePath, a)
    scanFile = sequence . (normaliseWithNoTrailing &&& scanner)

    isIgnored :: FilePath -> Bool
    isIgnored = matchesGlobPatterns root $ tcIgnored config

    -- Strip leading "." and trailing "/"
    location :: FilePath
    location =
      if root `equalFilePath` "."
        then ""
        else dropTrailingPathSeparator root

scanRepo
  :: MonadIO m
  => Rewrite -> FormatsSupport -> TraversalConfig -> FilePath -> m ScanResult
scanRepo rw formatsSupport config root = do
  putTextRewrite rw "Scanning repository..."

  when (not $ isDirectory root) $
    die $ "Repository's root does not seem to be a directory: " <> root

  (errs, fileInfos) <- liftIO
    $ (gatherScanErrs &&& gatherFileInfos)
    <$> readDirectoryWith config processFile root

  let dirs = fromList $ foldMap (getDirs . fst) fileInfos

  return . ScanResult errs $ RepoInfo (M.fromList fileInfos) dirs
  where
    isDirectory :: FilePath -> Bool
    isDirectory = readingSystem . doesDirectoryExist

    -- Get all directories from filepath.
    getDirs :: FilePath -> [FilePath]
    getDirs = scanl (</>) "" . splitDirectories . takeDirectory

    gatherScanErrs
      :: [(FilePath, Maybe (FileInfo, [ScanError]))]
      -> [ScanError]
    gatherScanErrs = fold . mapMaybe (fmap snd . snd)

    gatherFileInfos
      :: [(FilePath, Maybe (FileInfo, [ScanError]))]
      -> [(FilePath, Maybe FileInfo)]
    gatherFileInfos = map (second (fmap fst))

    processFile :: FilePath -> IO $ Maybe (FileInfo, [ScanError])
    processFile file = do
      let ext = takeExtension file
      let mscanner = formatsSupport ext
      forM mscanner ($ file)
