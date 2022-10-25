{- SPDX-FileCopyrightText: 2018-2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generalised repo scanner and analyser.

module Xrefcheck.Scan
  ( ExclusionConfig
  , ExclusionConfig' (..)
  , Extension
  , ScanAction
  , FormatsSupport
  , RepoInfo (..)
  , ScanError (..)
  , ScanErrorDescription (..)
  , ScanResult (..)

  , normaliseExclusionConfigFilePaths
  , scanRepo
  , specificFormatsSupport
  , ecIgnoredL
  , ecVirtualFilesL
  , ecNotScannedL
  , ecIgnoreRefsL
  ) where

import Universum

import Control.Lens (makeLensesWith)
import Data.Aeson (FromJSON (..), genericParseJSON, withText)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Reflection (Given)
import Fmt (Buildable (..), nameF, (+|), (|+))
import System.Directory (doesDirectoryExist)
import System.FilePath
  (dropTrailingPathSeparator, equalFilePath, splitDirectories, takeDirectory, takeExtension, (</>))
import System.Process (cwd, readCreateProcess, shell)
import Text.Regex.TDFA.Common (CompOption (..), ExecOption (..), Regex)
import Text.Regex.TDFA.Text qualified as R

import Xrefcheck.Core
import Xrefcheck.Progress
import Xrefcheck.System (RelGlobPattern, matchesGlobPatterns, normaliseGlobPattern, readingSystem)
import Xrefcheck.Util

-- | Type alias for ExclusionConfig' with all required fields.
type ExclusionConfig = ExclusionConfig' Identity

-- | Config of repositry exclusions.
data ExclusionConfig' f = ExclusionConfig
  { ecIgnored      :: Field f [RelGlobPattern]
    -- ^ Files and folders, files in which we completely ignore.
  , ecVirtualFiles :: Field f [RelGlobPattern]
    -- ^ Files which we pretend do exist.
  , ecNotScanned   :: Field f [RelGlobPattern]
    -- ^ Files, references in which we should not analyze.
  , ecIgnoreRefs   :: Field f [Regex]
    -- ^ Regular expressions that match external references we should not verify.
  } deriving stock (Generic)

makeLensesWith postfixFields ''ExclusionConfig'

normaliseExclusionConfigFilePaths :: ExclusionConfig -> ExclusionConfig
normaliseExclusionConfigFilePaths ec@ExclusionConfig{..}
  = ec
    { ecIgnored = map normaliseGlobPattern ecIgnored
    , ecVirtualFiles = map normaliseGlobPattern ecVirtualFiles
    , ecNotScanned = map normaliseGlobPattern ecNotScanned
    }

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
  :: forall a. ExclusionConfig
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
    isIgnored = matchesGlobPatterns root $ ecIgnored config

    -- Strip leading "." and trailing "/"
    location :: FilePath
    location =
      if root `equalFilePath` "."
        then ""
        else dropTrailingPathSeparator root

scanRepo
  :: MonadIO m
  => Rewrite -> FormatsSupport -> ExclusionConfig -> FilePath -> m ScanResult
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

-----------------------------------------------------------
-- Yaml instances
-----------------------------------------------------------

instance FromJSON Regex where
  parseJSON = withText "regex" $ \val -> do
    let errOrRegex = R.compile defaultCompOption defaultExecOption val
    either (error . show) return errOrRegex

-- Default boolean values according to
-- https://hackage.haskell.org/package/regex-tdfa-1.3.1.0/docs/Text-Regex-TDFA.html#t:CompOption
defaultCompOption :: CompOption
defaultCompOption = CompOption
  { caseSensitive = True
  , multiline = True
  , rightAssoc = True
  , newSyntax = True
  , lastStarGreedy = False
  }

-- ExecOption value to improve speed
defaultExecOption :: ExecOption
defaultExecOption = ExecOption {captureGroups = False}

instance FromJSON (ExclusionConfig' Maybe) where
  parseJSON = genericParseJSON aesonConfigOption

instance FromJSON (ExclusionConfig) where
  parseJSON = genericParseJSON aesonConfigOption
