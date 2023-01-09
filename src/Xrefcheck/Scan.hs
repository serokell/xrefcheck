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
  , ReadDirectoryMode(..)
  , ScanError (..)
  , ScanErrorDescription (..)
  , ScanResult (..)
  , ScanStage (..)

  , mkParseScanError
  , mkGatherScanError
  , scanRepo
  , specificFormatsSupport
  , defaultCompOption
  , defaultExecOption
  , ecIgnoreL
  , ecIgnoreLocalRefsToL
  , ecIgnoreRefsFromL
  , ecIgnoreExternalRefsToL
  , reportScanErrs
  ) where

import Universum

import Control.Lens (makeLensesWith)
import Data.Aeson (FromJSON (..), genericParseJSON, withText)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Reflection (Given)
import Fmt (Buildable (..), fmt)
import System.Directory (doesDirectoryExist, pathIsSymbolicLink)
import System.Process (cwd, readCreateProcess, shell)
import Text.Interpolation.Nyan
import Text.Regex.TDFA.Common (CompOption (..), ExecOption (..), Regex)
import Text.Regex.TDFA.Text qualified as R

import Xrefcheck.Core
import Xrefcheck.Progress
import Xrefcheck.System
import Xrefcheck.Util

-- | Type alias for ExclusionConfig' with all required fields.
type ExclusionConfig = ExclusionConfig' Identity

-- | Config of repositry exclusions.
data ExclusionConfig' f = ExclusionConfig
  { ecIgnore               :: Field f [CanonicalRelGlobPattern]
    -- ^ Files which we completely ignore.
  , ecIgnoreLocalRefsTo    :: Field f [CanonicalRelGlobPattern]
    -- ^ Files references to which we do not verify.
  , ecIgnoreRefsFrom       :: Field f [CanonicalRelGlobPattern]
    -- ^ Files, references in which we should not analyze.
  , ecIgnoreExternalRefsTo :: Field f [Regex]
    -- ^ Regular expressions that match external references we should not verify.
  } deriving stock (Generic)

makeLensesWith postfixFields ''ExclusionConfig'

-- | File extension, dot included.
type Extension = String

-- | Way to parse a file.
type ScanAction = FilePath -> RelPosixLink -> IO (FileInfo, [ScanError 'Parse])

-- | All supported ways to parse a file.
type FormatsSupport = Extension -> Maybe ScanAction

data ScanResult = ScanResult
  { srScanErrors :: [ScanError 'Gather]
  , srRepoInfo   :: RepoInfo
  }

-- | A scan error indexed by different process stages.
--
-- Within 'Parse', 'seFile' has no information because the same
-- file is being parsed.
--
-- Within 'Gather', 'seFile' stores the 'FilePath' corresponding
-- to the file in where the error was found.
data ScanError (a :: ScanStage) = ScanError
  { seFile        :: ScanStageFile a
  , sePosition    :: Position
  , seDescription :: ScanErrorDescription
  }

data ScanStage = Parse | Gather

type family ScanStageFile (a :: ScanStage) where
  ScanStageFile 'Parse = ()
  ScanStageFile 'Gather = RelPosixLink

deriving stock instance Show (ScanError 'Parse)
deriving stock instance Show (ScanError 'Gather)
deriving stock instance Eq (ScanError 'Parse)
deriving stock instance Eq (ScanError 'Gather)

-- | Make a 'ScanError' for the 'Parse' stage.
mkParseScanError :: Position -> ScanErrorDescription -> ScanError 'Parse
mkParseScanError = ScanError ()

-- | Promote a 'ScanError' from the 'Parse' stage
-- to the 'Gather' stage.
mkGatherScanError :: RelPosixLink ->  ScanError 'Parse -> ScanError 'Gather
mkGatherScanError seFile ScanError{sePosition, seDescription} = ScanError
  { seFile
  , sePosition
  , seDescription
  }

instance Given ColorMode => Buildable (ScanError 'Gather) where
  build ScanError{..} = [int||
    In file #{styleIfNeeded Faint (styleIfNeeded Bold seFile)}
    scan error #{sePosition}:

    #{seDescription}

    |]

reportScanErrs :: Given ColorMode => NonEmpty (ScanError 'Gather) -> IO ()
reportScanErrs errs = fmt
  [int||
  === Scan errors found ===

  #{interpolateIndentF 2 (interpolateBlockListF' "➥ " build errs)}
  Scan errors dumped, #{length errs} in total.
  |]

data ScanErrorDescription
  = LinkErr
  | FileErr
  | ParagraphErr Text
  | UnrecognisedErr Text
  deriving stock (Show, Eq)

instance Buildable ScanErrorDescription where
  build = \case
    LinkErr -> [int||Expected a LINK after "ignore link" annotation|]
    FileErr -> [int||Annotation "ignore all" must be at the top of \
                     markdown or right after comments at the top|]
    ParagraphErr txt -> [int||Expected a PARAGRAPH after \
                              "ignore paragraph" annotation, but found #{txt}|]
    UnrecognisedErr txt -> [int||Unrecognised option "#{txt}" perhaps you meant \
                                 <"ignore link"|"ignore paragraph"|"ignore all">|]

specificFormatsSupport :: [([Extension], ScanAction)] -> FormatsSupport
specificFormatsSupport formats = \ext -> M.lookup ext formatsMap
  where
    formatsMap = M.fromList
        [ (extension, parser)
        | (extensions, parser) <- formats
        , extension <- extensions
        ]

data ReadDirectoryMode
  = RdmTracked
  -- ^ Consider files tracked by Git, obtained from "git ls-files"
  | RdmUntracked
  -- ^ Consider files that are not tracked nor ignored by Git, obtained from
  -- "git ls-files --others --exclude-standard"
  | RdmBothTrackedAndUtracked
  -- ^ Combine output from commands listed above, so we consider all files
  -- except ones that are explicitly ignored by Git

-- | Process files that match given @ReadDirectoryMode@ and aren't ignored by the config.
readDirectoryWith
  :: forall a. ReadDirectoryMode
  -> ExclusionConfig
  -> (RelPosixLink -> IO a)
  -> FilePath
  -> IO [(RelPosixLink, a)]
readDirectoryWith mode config scanner root = do
  relativeFiles <- fmap mkRelPosixLink . L.lines <$> getFiles
  traverse scanFile $ filter (not . isIgnored) relativeFiles

  where

    getFiles = case mode of
      RdmTracked -> getTrackedFiles
      RdmUntracked -> getUntrackedFiles
      RdmBothTrackedAndUtracked -> liftA2 (<>) getTrackedFiles getUntrackedFiles

    getTrackedFiles = readCreateProcess
      (shell "git ls-files"){cwd = Just root} ""
    getUntrackedFiles = readCreateProcess
      (shell "git ls-files --others --exclude-standard"){cwd = Just root} ""

    scanFile :: RelPosixLink -> IO (RelPosixLink, a)
    scanFile c = (c,) <$> scanner c

    isIgnored :: RelPosixLink -> Bool
    isIgnored = matchesGlobPatterns (ecIgnore config) . canonicalizeRelPosixLink

scanRepo
  :: MonadIO m
  => ScanPolicy
  -> Rewrite
  -> FormatsSupport
  -> ExclusionConfig
  -> FilePath
  -> m ScanResult
scanRepo scanMode rw formatsSupport config root = do
  putTextRewrite rw "Scanning repository..."

  liftIO $ whenM (not <$> doesDirectoryExist root) $
    die $ "Repository's root does not seem to be a directory: " <> root

  (errs, processedFiles) <-
    let mode = case scanMode of
          OnlyTracked -> RdmTracked
          IncludeUntracked -> RdmBothTrackedAndUtracked
    in  liftIO $ (gatherScanErrs &&& gatherFileStatuses)
          <$> readDirectoryWith mode config processFile root

  notProcessedFiles <-  case scanMode of
    OnlyTracked -> liftIO $
      readDirectoryWith RdmUntracked config (const $ pure NotAddedToGit) root
    IncludeUntracked -> pure []

  let scannableNotProcessedFiles = filter (isJust . mscanner . fst) notProcessedFiles

  whenJust (nonEmpty $ map fst scannableNotProcessedFiles) $ \files -> hPutStrLn @Text stderr
    [int|A|
    Those files are not added by Git, so we're not scanning them:
    #{interpolateBlockListF files}
    Please run "git add" before running xrefcheck or enable \
    --include-untracked CLI option to check these files.
    |]

  let trackedDirs = foldMap (getIntermediateDirs . fst) processedFiles
      untrackedDirs = foldMap (getIntermediateDirs . fst) notProcessedFiles

  return . ScanResult errs $ RepoInfo
    { riFiles = M.fromList $ fmap canonicalLinkEntry $ processedFiles <> notProcessedFiles
    , riDirectories = M.fromList $ fmap canonicalLinkEntry (fmap (, TrackedDirectory) trackedDirs
        <> fmap (, UntrackedDirectory) untrackedDirs)
    }
  where
    mscanner :: RelPosixLink -> Maybe ScanAction
    mscanner = formatsSupport . takeExtension

    gatherScanErrs
      :: [(RelPosixLink, (FileStatus, [ScanError 'Parse]))]
      -> [ScanError 'Gather]
    gatherScanErrs = foldMap $ \(file, (_, errs)) ->
      mkGatherScanError file <$> errs

    gatherFileStatuses
      :: [(RelPosixLink, (FileStatus, [ScanError 'Parse]))]
      -> [(RelPosixLink, FileStatus)]
    gatherFileStatuses = map (second fst)

    processFile :: RelPosixLink -> IO (FileStatus, [ScanError 'Parse])
    processFile file =
      ifM (pathIsSymbolicLink (filePathFromRoot root file))
      (pure (NotScannable, []))
      case mscanner file of
        Nothing -> pure (NotScannable, [])
        Just scanner -> scanner root file <&> _1 %~ Scanned

    canonicalLinkEntry
      :: (RelPosixLink, a)
      -> (CanonicalRelPosixLink, (RelPosixLink, a))
    canonicalLinkEntry (a, b) = (canonicalizeRelPosixLink a, (a, b))

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
