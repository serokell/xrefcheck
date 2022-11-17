{- SPDX-FileCopyrightText: 2018-2020 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE PatternSynonyms #-}

-- | Various primitives.

module Xrefcheck.Core where

import Universum

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON (..), withText)
import Data.Char (isAlphaNum)
import Data.Char qualified as C
import Data.Default (Default (..))
import Data.DList (DList)
import Data.DList qualified as DList
import Data.List qualified as L
import Data.Reflection (Given)
import Data.Text qualified as T
import Fmt (Buildable (..))
import System.FilePath (isPathSeparator, pathSeparator)
import Text.Interpolation.Nyan
import Time (Second, Time)

import Xrefcheck.Progress
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
  deriving stock (Show)

allFlavors :: [Flavor]
allFlavors = [GitHub, GitLab]
  where
    _exhaustivenessCheck = \case
      GitHub -> ()
      GitLab -> ()
      -- if you update this, also update the list above

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
  { rName   :: Text
    -- ^ Text displayed as reference.
  , rLink   :: Text
    -- ^ File or site reference points to.
  , rAnchor :: Maybe Text
    -- ^ Section or custom anchor tag.
  , rPos    :: Position
  } deriving stock (Show, Generic)

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

instance Default FileInfo where
  def = diffToFileInfo mempty

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
  -- ^ Files that are not supported by our scanners
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
 { riFiles       :: Map FilePath FileStatus
   -- ^ Files from the repo with `FileInfo` attached to files that we've scanned.
 , riDirectories :: Map FilePath DirectoryStatus
   -- ^ Directories containing those files.
 } deriving stock (Show)

-----------------------------------------------------------
-- Instances
-----------------------------------------------------------

instance NFData Position
instance NFData Reference
instance NFData AnchorType
instance NFData Anchor
instance NFData FileInfo

instance Given ColorMode => Buildable Reference where
  build Reference{..} =
    [int||
    reference #{paren . build $ locationType rLink} #{rPos}:
      - text: #s{rName}
      - link: #{if null rLink then "-" else rLink}
      - anchor: #{rAnchor ?: styleIfNeeded Faint "-"}
    |]

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
  build (RepoInfo m _)
    | Just scanned <- nonEmpty [(name, info) | (name, Scanned info) <- toPairs m]
    = interpolateBlockListF' "⮚" buildFileReport scanned
    where
      buildFileReport (name, info) =
        [int||
        #{colorIfNeeded Cyan $ name}:
        #{info}
        |]
  build _ = "No scannable files found."

-----------------------------------------------------------
-- Analysing
-----------------------------------------------------------

pattern PathSep :: Char
pattern PathSep <- (isPathSeparator -> True)

-- | Type of reference.
data LocationType
  = CurrentFileLoc
    -- ^ Reference to this file, e.g. @[a](#header)@
  | RelativeLoc
    -- ^ Reference to a file relative to given one, e.g. @[b](folder/file#header)@
  | AbsoluteLoc
    -- ^ Reference to a file relative to the root, e.g. @[c](/folder/file#header)@
  | ExternalLoc
    -- ^ Reference to a file at outer site, e.g @[d](http://www.google.com/doodles)@
  | OtherLoc
    -- ^ Entry not to be processed, e.g. @mailto:e-mail@
  deriving stock (Eq, Show)

instance Given ColorMode => Buildable LocationType where
  build = \case
    CurrentFileLoc -> colorIfNeeded Green "current file"
    RelativeLoc    -> colorIfNeeded Yellow "relative"
    AbsoluteLoc    -> colorIfNeeded Blue "absolute"
    ExternalLoc    -> colorIfNeeded Red "external"
    OtherLoc       -> ""

-- | Whether this is a link to external resource.
isExternal :: LocationType -> Bool
isExternal = \case
  ExternalLoc -> True
  _ -> False

-- | Whether this is a link to repo-local resource.
isLocal :: LocationType -> Bool
isLocal = \case
  CurrentFileLoc -> True
  RelativeLoc    -> True
  AbsoluteLoc    -> True
  ExternalLoc    -> False
  OtherLoc       -> False

-- | Get type of reference.
locationType :: Text -> LocationType
locationType location = case toString location of
  []                      -> CurrentFileLoc
  PathSep : _             -> AbsoluteLoc
  '.' : PathSep : _       -> RelativeLoc
  '.' : '.' : PathSep : _ -> RelativeLoc
  _ | hasUrlProtocol      -> ExternalLoc
    | hasProtocol         -> OtherLoc
    | otherwise           -> RelativeLoc
  where
    hasUrlProtocol = "://" `T.isInfixOf` T.take 10 location
    hasProtocol = ":" `T.isInfixOf` T.take 10 location

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

-- | Strip './' prefix from local references.
canonizeLocalRef :: Text -> Text
canonizeLocalRef ref =
  maybe ref canonizeLocalRef (T.stripPrefix localPrefix ref)
  where
    localPrefix = toText ['.', pathSeparator]

-----------------------------------------------------------
-- Visualisation
-----------------------------------------------------------

data VerifyProgress = VerifyProgress
  { vrLocal    :: !(Progress Int)
  , vrExternal :: !(Progress Int)
  } deriving stock (Show)

initVerifyProgress :: [Reference] -> VerifyProgress
initVerifyProgress references = VerifyProgress
  { vrLocal = initProgress (length localRefs)
  , vrExternal = initProgress (length (ordNub $ map rLink extRefs))
  }
  where
    (extRefs, localRefs) = L.partition (isExternal . locationType . rLink) references

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
