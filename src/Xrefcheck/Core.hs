{- SPDX-FileCopyrightText: 2018-2020 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE PatternSynonyms #-}

-- | Various primitives.

module Xrefcheck.Core where

import Control.Lens (makeLenses, (%=))
import Data.Aeson (FromJSON (..), withText)
import Data.Char (isAlphaNum)
import qualified Data.Char as C
import Data.Default (Default (..))
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import Fmt (Buildable (..), blockListF, blockListF', nameF, (+|), (|+))
import System.Console.Pretty (Color (..), Style (..), color, style)
import System.FilePath (isPathSeparator, pathSeparator)
import Text.Numeral.Roman (toRoman)

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
  deriving (Show)

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
    deriving (Show, Generic)

instance Buildable Position where
    build (Position pos) = case pos of
        Nothing -> ""
        Just p  -> style Faint $ "at src:" <> build p

-- | Full info about a reference.
data Reference = Reference
    { rName   :: Text
      -- ^ Text displayed as reference.
    , rLink   :: Text
      -- ^ File or site reference points to.
    , rAnchor :: Maybe Text
      -- ^ Section or custom anchor tag.
    , rPos    :: Position
    } deriving (Show, Generic)

-- | Context of anchor.
data AnchorType
    = HeaderAnchor Int
      -- ^ Every section header is usually an anchor
    | HandAnchor
      -- ^ They can be set up manually
    | BiblioAnchor
      -- ^ Id of entry in bibliography
    deriving (Show, Eq, Generic)

-- | A referable anchor.
data Anchor = Anchor
    { aType :: AnchorType
    , aName :: Text
    , aPos  :: Position
    } deriving (Show, Generic)

-- | All information regarding a single file we care about.
data FileInfo = FileInfo
    { _fiReferences :: [Reference]
    , _fiAnchors    :: [Anchor]
    } deriving (Show, Generic)
makeLenses ''FileInfo

instance Default FileInfo where
    def = FileInfo [] []

newtype RepoInfo = RepoInfo (Map FilePath FileInfo)
    deriving (Show)

finaliseFileInfo :: FileInfo -> FileInfo
finaliseFileInfo = execState $ do
    fiReferences %= reverse
    fiAnchors %= reverse

-----------------------------------------------------------
-- Instances
-----------------------------------------------------------

instance NFData Position
instance NFData Reference
instance NFData AnchorType
instance NFData Anchor
instance NFData FileInfo

instance Buildable Reference where
    build Reference{..} =
        nameF ("reference " +| paren (build loc) |+ " " +| rPos |+ "") $
            blockListF
            [ "text: " <> show rName
            , "link: " <> build rLink
            , "anchor: " <> build (rAnchor ?: style Faint "-")
            ]
      where
        loc = locationType rLink

instance Buildable AnchorType where
    build = style Faint . \case
        HeaderAnchor l -> color Green ("header " <> toRoman l)
        HandAnchor -> color Yellow "hand made"
        BiblioAnchor -> color Cyan "biblio"

instance Buildable Anchor where
    build (Anchor t a p) = a |+ " (" +| t |+ ") " +| p |+ ""

instance Buildable FileInfo where
    build FileInfo{..} =
        blockListF
        [ nameF "references" $ blockListF _fiReferences
        , nameF "anchors" $ blockListF _fiAnchors
        ]

instance Buildable RepoInfo where
    build (RepoInfo m) =
        blockListF' "⮚" buildFileReport (M.toList m)
      where
        buildFileReport (name, info) = mconcat
            [ color Cyan $ fromString name <> ":\n"
            , build info
            , "\n"
            ]

-----------------------------------------------------------
-- Analysing
-----------------------------------------------------------

pattern PathSep :: Char
pattern PathSep <- (isPathSeparator -> True)

-- | Type of reference.
data LocationType
    = LocalLoc
      -- ^ Reference on this file
    | RelativeLoc
      -- ^ Reference to a file relative to given one
    | AbsoluteLoc
      -- ^ Reference to a file relative to the root
    | ExternalLoc
      -- ^ Reference to a file at outer site
    | OtherLoc
      -- ^ Entry not to be processed (e.g. "mailto:e-mail")
    deriving (Show)

instance Buildable LocationType where
    build = \case
        LocalLoc -> color Green "local"
        RelativeLoc -> color Yellow "relative"
        AbsoluteLoc -> color Blue "absolute"
        ExternalLoc -> color Red "external"
        OtherLoc -> ""

-- | Whether this is a link to external resource.
isExternal :: LocationType -> Bool
isExternal = \case
    ExternalLoc -> True
    _ -> False

-- | Whether this is a link to repo-local resource.
isLocal :: LocationType -> Bool
isLocal = \case
    LocalLoc -> True
    RelativeLoc -> True
    AbsoluteLoc -> True
    ExternalLoc -> False
    OtherLoc -> False

-- | Get type of reference.
locationType :: Text -> LocationType
locationType location = case toString location of
    []                      -> LocalLoc
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
    case T.stripPrefix localPrefix ref of
      Nothing -> ref
      Just r  -> canonizeLocalRef r
  where
    localPrefix = toText ['.', pathSeparator]

-----------------------------------------------------------
-- Visualisation
-----------------------------------------------------------

data VerifyProgress = VerifyProgress
    { vrLocal    :: !(Progress Int)
    , vrExternal :: !(Progress Int)
    } deriving (Show)

initVerifyProgress :: [Reference] -> VerifyProgress
initVerifyProgress references =
    VerifyProgress
    { vrLocal = initProgress (length localRefs)
    , vrExternal = initProgress (length extRefs)
    }
  where
    (extRefs, localRefs) =
        L.partition isExternal $
        map (locationType . rLink) references

showAnalyseProgress :: VerifyMode -> VerifyProgress -> Text
showAnalyseProgress mode VerifyProgress{..} = mconcat . mconcat $
    [ [ "Verifying " ]
    , [ showProgress "local" 10 White vrLocal <> " "
      | shouldCheckLocal mode ]
    , [ showProgress "external" 15 Yellow vrExternal
      | shouldCheckExternal mode ]
    ]

reprintAnalyseProgress :: Rewrite -> VerifyMode -> VerifyProgress -> IO ()
reprintAnalyseProgress rw mode p = putTextRewrite rw (showAnalyseProgress mode p)
