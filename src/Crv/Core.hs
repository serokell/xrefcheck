-- | Various primitives.

module Crv.Core where

import Control.DeepSeq (NFData)
import Control.Lens (makeLenses, (%=))
import Data.Char (isAlphaNum)
import Data.Default (Default (..))
import qualified Data.Map as M
import qualified Data.Text as T
import Fmt (Buildable (..), blockListF, blockListF', nameF, (+|), (|+))
import System.Console.Pretty (Color (..), Style (..), color, style)
import Text.Numeral.Roman (toRoman)

import Crv.Util ()

-----------------------------------------------------------
-- Types
-----------------------------------------------------------

-- | Full info about a reference.
data Reference = Reference
    { rName   :: Text
      -- ^ Text displayed as reference.
    , rLink   :: Text
      -- ^ File or site reference points to.
    , rAnchor :: Maybe Text
      -- ^ Section or custom anchor tag.
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

instance NFData Reference
instance NFData AnchorType
instance NFData Anchor
instance NFData FileInfo

instance Buildable Reference where
    build Reference{..} = nameF ("reference (" +| build loc |+ ")") $
        blockListF
        [ "text: " <> show rName
        , "link: " <> rLink
        , "anchor: " <> (rAnchor ?: style Faint "-")
        ]
      where
        loc = locationType rLink

instance Buildable AnchorType where
    build = style Faint . \case
        HeaderAnchor l -> color Green ("header " <> toRoman l)
        HandAnchor -> color Yellow "hand made"
        BiblioAnchor -> color Cyan "biblio"

instance Buildable Anchor where
    build (Anchor t a) = a |+ " (" +| t |+ ")"

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
    deriving (Show)

instance Buildable LocationType where
    build = \case
        LocalLoc -> color Green "local"
        RelativeLoc -> color Yellow "relative"
        AbsoluteLoc -> color Blue "absolute"
        ExternalLoc -> color Red "external"

-- | Get type of reference.
locationType :: Text -> LocationType
locationType location = case toString location of
    []                  -> LocalLoc
    '/' : _             -> AbsoluteLoc
    '.' : '/' : _       -> RelativeLoc
    '.' : '.' : '/' : _ -> RelativeLoc
    _ | hasProtocol     -> ExternalLoc
    _                   -> RelativeLoc
  where
    hasProtocol = "://" `T.isInfixOf` (T.take 10 location)

-- | Convert section header name to an anchor refering it.
-- Conversion rules: https://docs.gitlab.com/ee/user/markdown.html#header-ids-and-links
headerToAnchor :: Text -> Text
headerToAnchor =
    T.filter (\c -> isAlphaNum c || c == '-') .
    T.replace " " "-" .
    T.replace "+" "-" .
    T.toLower
