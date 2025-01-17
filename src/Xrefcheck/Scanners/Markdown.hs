{- SPDX-FileCopyrightText: 2018-2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Scanner for gathering references to verify from Markdown documents.
module Xrefcheck.Scanners.Markdown
  ( MarkdownConfig (..)

  , defGithubMdConfig
  , markdownScanner
  , markdownSupport
  , parseFileInfo
  , makeError
  ) where

import Universum hiding (use)

import CMarkGFM
  (Node (..), NodeType (..), PosInfo (..), commonmarkToNode, extAutolink, optFootnotes)
import Control.Lens (_Just, makeLenses, makeLensesFor, use, (.=))
import Control.Monad.Trans.Writer.CPS (Writer, runWriter, tell)
import Data.Aeson (FromJSON (..), genericParseJSON)
import Data.ByteString qualified as BS
import Data.DList qualified as DList
import Data.Reflection (Given)
import Data.Text qualified as T
import Fmt (Buildable (..), nameF)
import Text.HTML.TagSoup
import Text.Interpolation.Nyan

import Xrefcheck.Core
import Xrefcheck.Scan
import Xrefcheck.System
import Xrefcheck.Util

data MarkdownConfig = MarkdownConfig
  { mcFlavor :: Flavor
  } deriving stock (Generic)

instance FromJSON (MarkdownConfig) where
  parseJSON = genericParseJSON aesonConfigOption

defGithubMdConfig :: MarkdownConfig
defGithubMdConfig = MarkdownConfig
  { mcFlavor = GitHub
  }

instance Buildable Node where
  build (Node _mpos ty mSubs) = nameF (show ty) $
    maybe "[]" interpolateBlockListF (nonEmpty mSubs)

toPosition :: FilePath -> Maybe PosInfo -> Position
toPosition filepath = Position . \case
  Nothing -> [int|s|#{filepath}|]
  Just PosInfo{..}
    | startLine == endLine ->
        [int|s|
        #{filepath}:#{startLine}:#{startColumn}-#{endColumn}
        |]
    | otherwise ->
        [int|s|
        #{filepath}:#{startLine}:#{startColumn}-#{endLine}:#{endColumn}
        |]

-- | Extract text from the topmost node.
nodeExtractText :: Node -> Text
nodeExtractText = T.strip . mconcat . map extractText . nodeFlatten
  where
    extractText = \case
      TEXT t -> t
      CODE t -> t
      _ -> ""

    nodeFlatten :: Node -> [NodeType]
    nodeFlatten (Node _pos ty subs) = ty : concatMap nodeFlatten subs


data IgnoreMode
  = IMLink
  | IMParagraph
  | IMAll
  deriving stock (Eq)

-- | "ignore link" pragmas in different places behave slightly different,
-- so @IgnoreMode@ @Link@ is parametrized
data IgnoreLinkState
  = ExpectingLinkInParagraph
  -- ^ When ignore annotation is inside @PARAGRAPH@ node,
  -- we expect a link to ignore later in this paragraph.
  -- We raise scan error if we see this status after
  -- traversing subnodes of a @PARAGRAPH@ node.
  | ExpectingLinkInSubnodes
  -- ^ If ignore annotation is not inside @PARAGRAPH@, then we expect a link
  -- in subtree of next node. We raise scan error if we see this status
  -- after traversing childs of any node that is not an ignore annotation.
  | ParentExpectsLink
  -- ^ When we have `ExpectingLinkInSubnodes`, we traverse subtree of some node,
  -- and we should change `IgnoreLinkState`, because it's not a problem if
  -- our node's first child doesn't contain a link. So this status means that
  -- we won't throw errors if we don't find a link for now
  deriving stock (Eq)

data IgnoreModeState
  = IMSLink IgnoreLinkState
  | IMSParagraph
  | IMSAll
  deriving stock (Eq)

-- | Bind `IgnoreMode` to its `PosInfo` so that we can tell where the
-- corresponding annotation was declared.
data Ignore = Ignore
  { _ignoreMode :: IgnoreModeState
  , _ignorePos :: Maybe PosInfo
  }
makeLensesFor [("_ignoreMode", "ignoreMode")] 'Ignore

data GetIgnoreMode
  = NotAnAnnotation
  | ValidMode IgnoreMode
  | InvalidMode Text
  deriving stock (Eq)

data ScannerState = ScannerState
  { _ssIgnore :: Maybe Ignore
  , _ssParentNodeType :: Maybe NodeType
  -- ^ @cataNodeWithParentNodeInfo@ allows to get a @NodeType@ of parent node from this field
  }
makeLenses ''ScannerState

initialScannerState :: ScannerState
initialScannerState = ScannerState
  { _ssIgnore = Nothing
  , _ssParentNodeType = Nothing
  }

type ScannerM a = StateT ScannerState (Writer [ScanError 'Parse]) a

-- | A fold over a `Node`.
cataNode :: (Maybe PosInfo -> NodeType -> [c] -> c) -> Node -> c
cataNode f (Node pos ty subs) = f pos ty (cataNode f <$> subs)

-- | Sets correct @_ssParentNodeType@ before running scanner on each node
cataNodeWithParentNodeInfo
  :: (Maybe PosInfo -> NodeType -> [ScannerM a] -> ScannerM a)
  -> Node
  -> ScannerM a
cataNodeWithParentNodeInfo f node = cataNode f' node
  where
    f' pos ty childScanners = f pos ty $
      map (ssParentNodeType .= Just ty >>) childScanners

-- | Find ignore annotations (ignore paragraph and ignore link)
-- and remove nodes that should be ignored.
removeIgnored :: Node -> ExtractorM Node
removeIgnored rootNode = do
  filepath <- asks ecFilePath
  let
    remove
      :: Maybe PosInfo
      -> NodeType
      -> [ScannerM Node]
      -> ScannerM Node
    remove pos ty subs = do
      let node = Node pos ty []
      scan <- use ssIgnore >>= \case
        -- When no `Ignore` state is set check next node for annotation,
        -- if found then set it as new `IgnoreMode` otherwise skip node.
        Nothing -> handleIgnoreMode pos ty subs $ getIgnoreMode node
        Just (Ignore mode modePos) ->
          case (mode, ty) of
            -- We expect to find a paragraph immediately after the
            -- `ignore paragraph` annotanion. If the paragraph is not
            -- found we should report an error.
            (IMSParagraph, PARAGRAPH)    -> (ssIgnore .= Nothing) $> defNode
            (IMSParagraph, x)            -> do
              lift . tell $ makeError filepath modePos (ParagraphErr (prettyType x))
              ssIgnore .= Nothing
              Node pos ty <$> sequence subs

            -- We don't expect to find an `ignore all` annotation here,
            -- since that annotation should be at the top of the file and
            -- the file should already be ignored when `checkIgnoreFile` is called.
            -- We should report an error if we find it anyway.
            (IMSAll, _)                 -> do
              lift . tell $ makeError filepath modePos FileErr
              ssIgnore .= Nothing
              Node pos ty <$> sequence subs

            (IMSLink _, LINK {})         -> do
              ssIgnore .= Nothing
              return defNode
            (IMSLink _, IMAGE {})        -> do
              ssIgnore .= Nothing
              return defNode
            (IMSLink ignoreLinkState, _) -> do
              when (ignoreLinkState == ExpectingLinkInSubnodes) $
                ssIgnore . _Just . ignoreMode .=  IMSLink ParentExpectsLink
              node' <- Node pos ty <$> sequence subs
              when (ignoreLinkState == ExpectingLinkInSubnodes) $ do
                currentIgnore <- use ssIgnore
                case currentIgnore of
                  Just (Ignore {_ignoreMode = IMSLink ParentExpectsLink}) -> do
                    lift $ tell $ makeError filepath modePos LinkErr
                    ssIgnore .= Nothing
                  _ -> pass
              return node'

      when (ty == PARAGRAPH) $ use ssIgnore >>= \case
        Just (Ignore (IMSLink ExpectingLinkInParagraph) pragmaPos) ->
          lift $ tell $ makeError filepath pragmaPos LinkErr
        _ -> pass

      return scan

    handleIgnoreMode
      :: Maybe PosInfo
      -> NodeType
      -> [ScannerM Node]
      -> GetIgnoreMode
      -> ScannerM Node
    handleIgnoreMode pos nodeType subs = \case
      ValidMode mode  -> do
        ignoreModeState <- case mode of
          IMLink -> use ssParentNodeType <&> IMSLink . \case
             Just PARAGRAPH -> ExpectingLinkInParagraph
             _ -> ExpectingLinkInSubnodes

          IMParagraph -> pure IMSParagraph

          IMAll -> pure IMSAll

        (ssIgnore .= Just (Ignore ignoreModeState correctPos)) $> defNode
      InvalidMode msg -> do
        lift . tell $ makeError filepath correctPos $ UnrecognisedErr msg
        (ssIgnore .= Nothing) $> defNode
      NotAnAnnotation -> Node pos nodeType <$> sequence subs
      where
        correctPos = getPosition $ Node pos nodeType []

    prettyType :: NodeType -> Text
    prettyType ty =
      let mType = safeHead $ words $ show ty
      in fromMaybe "" mType

    action :: ScannerM Node
    action = cataNodeWithParentNodeInfo remove rootNode

  (node, s) <- lift $ runStateT action initialScannerState
  case s of
    -- We expect `Ignore` state to be `Nothing` when we reach EOF,
    -- otherwise that means there was an annotation that didn't match
    -- any node, so we have to report that.
    ScannerState {_ssIgnore = Just (Ignore mode pos)} -> do
      case mode of
        IMSParagraph -> do
            lift $ tell . makeError filepath pos $ ParagraphErr "EOF"
            pure node
        IMSLink _ -> do
            lift $ tell $ makeError filepath pos LinkErr
            pure node
        IMSAll -> do
            lift $ tell $ makeError filepath pos FileErr
            pure node
    _ -> pure node

-- | Custom `foldMap` for source tree.
foldNode :: (Monoid a, Monad m) => (Node -> m a) -> Node -> m a
foldNode action node@(Node _ _ subs) = do
  a <- action node
  b <- concatForM subs (foldNode action)
  return (a <> b)

data ExtractorCtx = ExtractorCtx
  { ecConfig :: MarkdownConfig
  , ecFilePath :: String  -- for printing
  }

type ExtractorM a = ReaderT ExtractorCtx (Writer [ScanError 'Parse]) a

-- | Extract information from source tree.
nodeExtractInfo :: Node -> ExtractorM FileInfo
nodeExtractInfo input@(Node _ _ nSubs) = do
  if checkIgnoreAllFile nSubs
  then return (diffToFileInfo mempty)
  else diffToFileInfo <$> (foldNode extractor =<< removeIgnored input)

  where
    extractor :: Node -> ExtractorM FileInfoDiff
    extractor node@(Node pos ty _) = do
      filepath <- asks ecFilePath
      case ty of
        HTML_BLOCK _ -> do
          return mempty

        HEADING lvl -> do
          flavor <- asks (mcFlavor . ecConfig)
          let aType = HeaderAnchor lvl
          let aName = headerToAnchor flavor $ nodeExtractText node
          let aPos  = toPosition filepath pos
          return $ FileInfoDiff DList.empty $ DList.singleton $ Anchor {aType, aName, aPos}

        HTML_INLINE text -> do
          let
            mName = do
              tag <- safeHead $ parseTags text
              attributes <- case tag of
                TagOpen a attrs
                  | T.toLower a == "a" -> Just attrs
                _ -> Nothing
              (_, name) <- find (\(field, _) -> T.toLower field `elem` ["name", "id"]) attributes
              pure name

          case mName of
            Just aName -> do
              let aType = HandAnchor
                  aPos  = toPosition filepath pos
              return $ FileInfoDiff
                mempty
                (pure $ Anchor {aType, aName, aPos})

            Nothing -> do
              return mempty

        LINK url _ -> extractLink url

        IMAGE url _ -> extractLink url

        _ -> return mempty

      where
        extractLink url = do
          filepath <- asks ecFilePath
          let rName = nodeExtractText node
              rPos = toPosition filepath pos
              rInfo = referenceInfo $ if null url then rName else url

          return $ FileInfoDiff
            (DList.singleton $ Reference {rName, rPos, rInfo})
            DList.empty

-- | Check if there is `ignore all` at the beginning of the file,
-- ignoring preceding comments if there are any.
checkIgnoreAllFile :: [Node] -> Bool
checkIgnoreAllFile nodes =
  let isSimpleComment :: Node -> Bool
      isSimpleComment node = isComment node && not (isIgnoreFile node)

      mIgnoreFile = safeHead $ dropWhile isSimpleComment nodes
  in maybe False isIgnoreFile mIgnoreFile
  where
    isComment :: Node -> Bool
    isComment = isJust . getCommentContent

    isIgnoreFile :: Node -> Bool
    isIgnoreFile = (ValidMode IMAll ==) . getIgnoreMode

defNode :: Node
defNode = Node Nothing DOCUMENT [] -- hard-coded default Node

makeError
  :: FilePath
  -> Maybe PosInfo
  -> ScanErrorDescription
  -> [ScanError 'Parse]
makeError filepath pos errDescription =
  one $ mkParseScanError (toPosition filepath pos) errDescription

getCommentContent :: Node -> Maybe Text
getCommentContent node = do
  txt <- getHTMLText node
  T.stripSuffix "-->" =<< T.stripPrefix "<!--" (T.strip txt)

getHTMLText :: Node -> Maybe Text
getHTMLText (Node _ (HTML_BLOCK txt) _) = Just txt
getHTMLText (Node _ (HTML_INLINE txt) _) = Just txt
getHTMLText _ = Nothing

getXrefcheckContent :: Node -> Maybe Text
getXrefcheckContent node =
  let notStripped = T.stripPrefix "xrefcheck:" . T.strip =<<
        getCommentContent node
  in T.strip <$> notStripped

-- | Get the correct position of an annotation node. There is a bug in
-- `commonmarkToNode` from the `cmark-gfm` package that affects one line
-- `HTML_BLOCK` nodes, those node have wrong end line and end column positions.
-- As our annotations are always oneliners, we can fix this by simply setting
-- end line equals to start line and calculating end column from start column
-- and annotation length.
getPosition :: Node -> Maybe PosInfo
getPosition node@(Node pos _ _) = do
  annLength <- length . T.strip <$> getHTMLText node
  PosInfo sl sc _ _ <- pos
  pure $ PosInfo sl sc sl (sc + annLength - 1)

-- | Extract `IgnoreMode` if current node is xrefcheck annotation.
getIgnoreMode :: Node -> GetIgnoreMode
getIgnoreMode node = maybe NotAnAnnotation (textToMode . words) (getXrefcheckContent node)

textToMode :: [Text] -> GetIgnoreMode
textToMode ("ignore" : [x])
  | x == "link"      = ValidMode IMLink
  | x == "paragraph" = ValidMode IMParagraph
  | x == "all"      = ValidMode IMAll
  | otherwise        = InvalidMode x
textToMode _         = NotAnAnnotation

parseFileInfo :: MarkdownConfig -> String -> T.Text -> (FileInfo, [ScanError 'Parse])
parseFileInfo config pathForPrinting input
  = runWriter
  $ flip runReaderT (ExtractorCtx config pathForPrinting)
  $ nodeExtractInfo
  $ commonmarkToNode [optFootnotes] [extAutolink] input

markdownScanner :: Given PrintUnixPaths => MarkdownConfig -> ScanAction
markdownScanner config root relativePath =
  parseFileInfo config pathForPrinting . decodeUtf8
    <$> BS.readFile rootedPath
  where
    rootedPath = filePathFromRoot root relativePath
    pathForPrinting = mkPathForPrinting rootedPath

markdownSupport :: Given PrintUnixPaths => MarkdownConfig -> FileSupport
markdownSupport config isSymlink extension = do
  guard $ extension == ".md"
  guard $ not isSymlink
  pure $ markdownScanner config
