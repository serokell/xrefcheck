{- SPDX-FileCopyrightText: 2018-2020 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE PatternSynonyms #-}

-- | Various primitives.

module Xrefcheck.Core where

import Universum

import Control.Lens (folded, makeLenses, makePrisms, to, united)
import Data.Aeson (FromJSON (..), withText)
import Data.Char (isAlphaNum)
import Data.Char qualified as C
import Data.DList (DList)
import Data.DList qualified as DList
import Data.Map qualified as M
import Data.Reflection (Given)
import Data.Text qualified as T
import Fmt (Buildable (..), Builder)
import System.FilePath.Posix (isPathSeparator)
import Text.Interpolation.Nyan
import Time (Second, Time)

import Xrefcheck.Progress
import Xrefcheck.System
import Xrefcheck.Util

-----------------------------------------------------------
-- Types
-----------------------------------------------------------

-- | Markdown flavor.
--
-- Unfortunatelly, CMark renderers used on different sites slightly differ,
-- we have to account for that.
data Flavor
  = GitHub
  | GitLab
  deriving stock (Show, Enum, Bounded)

allFlavors :: [Flavor]
allFlavors = [minBound .. maxBound]

-- | Whether anchors are case-sensitive for a given Markdown flavour or not.
caseInsensitiveAnchors :: Flavor -> Bool
caseInsensitiveAnchors GitHub = True
caseInsensitiveAnchors GitLab = False

instance FromJSON Flavor where
  parseJSON = withText "flavor" $ \txt ->
    case T.toLower txt of
      "github" -> pure GitHub
      "gitlab" -> pure GitLab
      _ -> fail $ "Unknown flavor " <> show txt

-- | Description of element position in source file.
-- We keep this in text because scanners for different formats use different
-- representation of this thing, and it actually appears in reports only.
newtype Position = Position (Maybe Text)
  deriving stock (Show, Eq, Generic)

instance Given ColorMode => Buildable Position where
  build (Position pos) = case pos of
    Nothing -> ""
    Just p  -> styleIfNeeded Faint $ "at src:" <> build p

-- | Full info about a reference.
data Reference = Reference
  { rName :: Text
    -- ^ Text displayed as reference.
  , rPos :: Position
    -- ^ Position in source file.
  , rInfo :: ReferenceInfo
    -- ^ More info about the reference.
  } deriving stock (Show, Generic)

-- | Info about the reference.
data ReferenceInfo
  = RIExternal ExternalLink
  | RIFile ReferenceInfoFile
  deriving stock (Show, Generic)

data ReferenceInfoFile = ReferenceInfoFile
  { rifAnchor :: Maybe Text
    -- ^ Section or custom anchor tag.
  , rifLink :: FileLink
    -- ^ More info about the link.
  } deriving stock (Show, Generic)

data ExternalLink
  = ELUrl Text
    -- ^ Reference to a file at outer site, e.g @[d](http://www.google.com/doodles)@.
  | ELOther Text
    -- ^ Entry not to be processed, e.g. @mailto:e-mail@.
  deriving stock (Show, Generic)

data FileLink
  = FLAbsolute RelPosixLink
    -- ^ Reference to a file or directory relative to the repository root.
  | FLRelative RelPosixLink
    -- ^ Reference to a file or directory relative to the given one.
  | FLLocal
    -- ^ Reference to this file.
  deriving stock (Show, Generic)

makePrisms ''ReferenceInfo
makePrisms ''ExternalLink

pattern PathSep :: Char
pattern PathSep <- (isPathSeparator -> True)

-- | Compute the 'ReferenceInfo' corresponding to a given link.
referenceInfo :: Text -> ReferenceInfo
referenceInfo url
  | hasUrlProtocol = RIExternal $ ELUrl url
  | hasProtocol = RIExternal $ ELOther url
  | null link = RIFile $ ReferenceInfoFile anchor FLLocal
  | otherwise = case T.uncons link of
      Just (PathSep, path) ->
        RIFile $ ReferenceInfoFile anchor $ FLAbsolute $ RelPosixLink path
      _ ->
        RIFile $ ReferenceInfoFile anchor $ FLRelative $ RelPosixLink link
  where
    hasUrlProtocol = "://" `T.isInfixOf` T.take 10 url
    hasProtocol = ":" `T.isInfixOf` T.take 10 url
    (link, anchor) = case T.splitOn "#" url of
      [t] -> (t, Nothing)
      t : ts -> (t, Just $ T.intercalate "#" ts)
      [] -> (url, Nothing)

-- | Context of anchor.
data AnchorType
  = HeaderAnchor Int
    -- ^ Every section header is usually an anchor
  | HandAnchor
    -- ^ They can be set up manually
  | BiblioAnchor
    -- ^ Id of entry in bibliography
  deriving stock (Show, Eq, Generic)

-- | A referable anchor.
data Anchor = Anchor
  { aType :: AnchorType
  , aName :: Text
  , aPos  :: Position
  } deriving stock (Show, Eq, Generic)

data FileInfoDiff = FileInfoDiff
  { _fidReferences :: DList Reference
  , _fidAnchors    :: DList Anchor
  }
makeLenses ''FileInfoDiff

diffToFileInfo :: FileInfoDiff -> FileInfo
diffToFileInfo (FileInfoDiff refs anchors) =
    FileInfo (DList.toList refs) (DList.toList anchors)

instance Semigroup FileInfoDiff where
  FileInfoDiff a b <> FileInfoDiff c d = FileInfoDiff (a <> c) (b <> d)

instance Monoid FileInfoDiff where
  mempty = FileInfoDiff mempty mempty

-- | All information regarding a single file we care about.
data FileInfo = FileInfo
  { _fiReferences :: [Reference]
  , _fiAnchors    :: [Anchor]
  } deriving stock (Show, Generic)
makeLenses ''FileInfo

data ScanPolicy
  = OnlyTracked
  -- ^ Scan and treat as existing only files tracked by Git.
  -- Warn when there are scannable files not added to Git yet.
  | IncludeUntracked
  -- ^ Also scan and treat as existing
  -- files that were neither tracked nor ignored by Git.
  deriving stock (Show, Eq)

data FileStatus
  = Scanned FileInfo
  | NotScannable
  -- ^ Files that are not supported by our scanners.
  | NotAddedToGit
  -- ^ We are not scanning files that are not added to git
  -- unless --include-untracked CLI option was enabled, but we're
  -- gathering information about them to improve reports.
  deriving stock (Show)

data DirectoryStatus
  = TrackedDirectory
  | UntrackedDirectory
  deriving stock (Show)

-- | All tracked files and directories.
data RepoInfo = RepoInfo
  { riFiles :: Map CanonicalRelPosixLink (RelPosixLink, FileStatus)
    -- ^ Files from the repo with `FileInfo` attached to files that we've scanned.
  , riDirectories :: Map CanonicalRelPosixLink (RelPosixLink, DirectoryStatus)
    -- ^ Directories containing those files.
  }

-- Search for a file in the repository.
lookupFile :: CanonicalRelPosixLink -> RepoInfo -> Maybe FileStatus
lookupFile path RepoInfo{..} =
  snd <$> M.lookup path riFiles

-- Search for a directory in the repository.
lookupDirectory :: CanonicalRelPosixLink -> RepoInfo -> Maybe DirectoryStatus
lookupDirectory path RepoInfo{..} =
  snd <$> M.lookup path riDirectories

-----------------------------------------------------------
-- Instances
-----------------------------------------------------------

instance NFData ReferenceInfo
instance NFData Anchor
instance NFData AnchorType
instance NFData ExternalLink
instance NFData FileInfo
instance NFData FileLink
instance NFData Position
instance NFData Reference
instance NFData ReferenceInfoFile

instance Given ColorMode => Buildable Reference where
  build Reference{..} =
    case rInfo of
      RIFile ReferenceInfoFile{..} ->
        case rifLink of
          FLLocal ->
            [int||
            reference #{paren $ colorIfNeeded Green "file-local"}#{posSep}#{rPos}:
              - text: #s{rName}
              - anchor: #{rifAnchor ?: styleIfNeeded Faint "-"}
            |]
          FLRelative link ->
            [int||
            reference #{paren $ colorIfNeeded Yellow "relative"}#{posSep}#{rPos}:
              - text: #s{rName}
              - link: #{link}
              - anchor: #{rifAnchor ?: styleIfNeeded Faint "-"}
            |]
          FLAbsolute link ->
            [int||
            reference #{paren $ colorIfNeeded Yellow "absolute"}#{posSep}#{rPos}:
              - text: #s{rName}
              - link: /#{link}
              - anchor: #{rifAnchor ?: styleIfNeeded Faint "-"}
            |]
      RIExternal (ELUrl url) ->
        [int||
        reference #{paren $ colorIfNeeded Red "external"}#{posSep}#{rPos}:
          - text: #s{rName}
          - link: #{url}
        |]
      RIExternal (ELOther url) ->
        [int||
        reference (other)#{posSep}#{rPos}:
          - text: #s{rName}
          - link: #{url}
        |]
    where
      posSep :: Text
      posSep = case rPos of
        Position Nothing -> ""
        _ -> " "

instance Given ColorMode => Buildable AnchorType where
  build = styleIfNeeded Faint . \case
    HeaderAnchor l -> colorIfNeeded Green ("header " <> headerLevelToRoman l)
    HandAnchor -> colorIfNeeded Yellow "hand made"
    BiblioAnchor -> colorIfNeeded Cyan "biblio"
    where
      headerLevelToRoman = \case
        1 -> "I"
        2 -> "II"
        3 -> "III"
        4 -> "IV"
        5 -> "V"
        6 -> "VI"
        n -> error "Bad header level: " <> show n

instance Given ColorMode => Buildable Anchor where
  build Anchor{..} =
    [int||
    #{aName} (#{aType}) #{aPos}
    |]

instance Given ColorMode => Buildable FileInfo where
  build FileInfo{..} =
    [int||
    - references:
    #{ interpolateIndentF 4 $ maybe "none" interpolateBlockListF (nonEmpty _fiReferences) }
    - anchors:
    #{ interpolateIndentF 4 $ maybe "none" interpolateBlockListF (nonEmpty _fiAnchors) }
    |]

instance Given ColorMode => Buildable RepoInfo where
  build RepoInfo{..}
    | Just scanned <- nonEmpty [(name, info) | (_, (name, Scanned info)) <- toPairs riFiles]
    = interpolateUnlinesF $ buildFileReport <$> scanned
    where
      buildFileReport :: (RelPosixLink, FileInfo) -> Builder
      buildFileReport (name, info) =
        [int||
        #{ colorIfNeeded Cyan name }:
        #{ interpolateIndentF 2 $ build info }
        |]
  build _ = "No scannable files found."

-----------------------------------------------------------
-- Analysing
-----------------------------------------------------------

-- | Which parts of verification do we perform.
data VerifyMode
  = LocalOnlyMode
  | ExternalOnlyMode
  | FullMode

shouldCheckLocal :: VerifyMode -> Bool
shouldCheckLocal = \case
  LocalOnlyMode -> True
  ExternalOnlyMode -> False
  FullMode -> True

shouldCheckExternal :: VerifyMode -> Bool
shouldCheckExternal = \case
  LocalOnlyMode -> False
  ExternalOnlyMode -> True
  FullMode -> True

-- | Convert section header name to an anchor refering it.
-- Conversion rules: https://docs.gitlab.com/ee/user/markdown.html#header-ids-and-links
headerToAnchor :: Flavor -> Text -> Text
headerToAnchor flavor = \t -> t
  & T.toLower
  & mergeSpecialSymbols
  where
    joinSubsequentChars sym = toText . go . toString
      where
        go = \case
          (c1 : c2 : s)
            | c1 == c2 && c1 == sym -> go (c1 : s)
          (c : s) -> c : go s
          [] -> []

    mergeSpecialSymbols = case flavor of
      GitLab -> \t -> t
        & T.replace " " "-"
        & T.filter (\c -> isAlphaNum c || c == '_' || c == '-')
        & joinSubsequentChars '-'
      GitHub ->
        -- GitHub case is tricky, it can produce many hythens in a row, e.g.
        -- "A - B" -> "a---b"
        let tmp = '\0'; tmpT = T.singleton tmp
        in \t -> t
        & T.replace " " tmpT
        & joinSubsequentChars tmp
        & T.replace tmpT "-"
        & T.filter (\c -> isAlphaNum c || c == '_' || c == '-')

-- | When there are several anchors with the same name, github automatically attaches
-- "-<number>" suffixes to duplications to make them referable unambiguously.
-- For instance, if there are two headers called "description", they would gain
-- "description" and "description-1" anchors correspondingly.
--
-- This function strips this suffix and returns the original anchor in case when
-- suffix is present.
stripAnchorDupNo :: Text -> Maybe Text
stripAnchorDupNo t = do
  let strippedNo = T.dropWhileEnd C.isNumber t
  guard (length strippedNo < length t)
  T.stripSuffix "-" strippedNo

-----------------------------------------------------------
-- Visualisation
-----------------------------------------------------------

data VerifyProgress = VerifyProgress
  { vrLocal    :: !(Progress Int ())
  , vrExternal :: !(Progress Int Text)
  } deriving stock (Show)

initVerifyProgress :: [Reference] -> VerifyProgress
initVerifyProgress references = VerifyProgress
  { vrLocal = initProgressWitnessed $
      references ^.. folded . to rInfo . (_RIFile . united <> _RIExternal . _ELOther . united)
  , vrExternal = initProgressWitnessed . ordNub $
      references ^.. folded . to rInfo . _RIExternal . _ELUrl
  }

showAnalyseProgress :: Given ColorMode => VerifyMode -> Time Second -> VerifyProgress -> Text
showAnalyseProgress mode posixTime VerifyProgress{..} =
  mconcat . mconcat $
    [ [ "Verifying " ]
    , [ showProgress "local" 10 White posixTime vrLocal <> " "
      | shouldCheckLocal mode ]
    , [ showProgress "external" 15 Yellow posixTime vrExternal
      | shouldCheckExternal mode ]
    ]

reprintAnalyseProgress :: Given ColorMode =>
  Rewrite -> VerifyMode -> Time Second -> VerifyProgress -> IO ()
reprintAnalyseProgress rw mode posixTime p = putTextRewrite rw $
  showAnalyseProgress mode posixTime p
