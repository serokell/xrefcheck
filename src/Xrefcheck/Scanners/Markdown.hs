{- SPDX-FileCopyrightText: 2018-2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Markdown documents markdownScanner.

module Xrefcheck.Scanners.Markdown
  ( MarkdownConfig (..)

  , defGithubMdConfig
  , markdownScanner
  , markdownSupport
  , parseFileInfo
  , makeError
  ) where

import Universum

import CMarkGFM (NodeType (..), PosInfo (..), commonmarkToNode, extAutolink, optFootnotes)
import CMarkGFM qualified as C
import Control.Lens (_Just, makeLenses, makeLensesFor, (.=))
import Control.Monad.Trans.Writer.CPS (Writer, runWriter, tell)
import Data.Aeson (FromJSON (..), genericParseJSON)
import Data.ByteString.Lazy qualified as BSL
import Data.DList qualified as DList
import Data.Default (def)
import Data.List (span)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Fmt (Buildable (..), nameF)
import Text.HTML.TagSoup
import Text.Interpolation.Nyan

import Xrefcheck.Core
import Xrefcheck.Scan
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

instance Buildable C.Node where
  build (C.Node _mpos ty mSubs) = nameF (show ty) $
    maybe "[]" interpolateBlockListF (nonEmpty mSubs)

toPosition :: Maybe PosInfo -> Position
toPosition = Position . \case
  Nothing -> Nothing
  Just PosInfo{..}
    | startLine == endLine -> Just $
        [int|s|
        #{startLine}:#{startColumn}-#{endColumn}
        |]
    | otherwise -> Just $
        [int|s|
        #{startLine}:#{startColumn}-#{endLine}:#{endColumn}
        |]

-- | Extract text from the topmost node.
nodeExtractText :: (C.Node) -> Text
nodeExtractText = T.strip . mconcat . map extractText . nodeFlatten
  where
    extractText = \case
      TEXT t -> t
      CODE t -> t
      _ -> ""

    nodeFlatten :: (C.Node) -> [NodeType]
    nodeFlatten (C.Node _pos ty subs) = ty : concatMap nodeFlatten subs


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
  deriving stock (Eq, Show)

data IgnoreModeState
  = IMSLink IgnoreLinkState
  | IMSParagraph
  deriving stock (Eq, Show)

-- | Bind `IgnoreMode` to its `PosInfo` so that we can tell where the
-- corresponding annotation was declared.
data Ignore = Ignore
  { _ignoreMode :: IgnoreModeState
  , _ignorePos :: Maybe PosInfo
  } deriving stock (Show)
makeLensesFor [("_ignoreMode", "ignoreMode")] 'Ignore

data Annotation
  = IgnoreAnnotation IgnoreMode
  | InvalidAnnotation Text
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

type ScannerM a = StateT ScannerState (Writer [ScanError]) a

-- | A fold over a `Node`.
cataNode :: (Maybe PosInfo -> NodeType -> [c] -> c) -> C.Node -> c
cataNode f (C.Node pos ty subs) = f pos ty (cataNode f <$> subs)

-- | Sets correct @_ssParentNodeType@ before running scanner on each node.
cataNodeWithParentNodeInfo
  :: (Maybe PosInfo -> NodeType -> [ScannerM a] -> ScannerM a)
  -> C.Node
  -> ScannerM a
cataNodeWithParentNodeInfo f node = cataNode f' node
  where
    f' pos ty childScanners = f pos ty $
      map (ssParentNodeType .= Just ty >>) childScanners

-- | Find ignore annotations (ignore paragraph and ignore link)
-- and remove nodes that should be ignored.
processAnnotations :: FilePath -> C.Node -> Writer [ScanError] C.Node
processAnnotations fp = withIgnoreMode . cataNodeWithParentNodeInfo process
  where
    process
      :: Maybe PosInfo
      -> NodeType
      -> [ScannerM C.Node]
      -> ScannerM C.Node
    process pos ty subs = do
      let node = C.Node pos ty []
      use ssIgnore >>= \ign -> do
        -- When no `Ignore` state is set check next node for annotation,
        -- if found then set it as new `IgnoreMode` otherwise skip node.
        let mbAnnotation = getAnnotation node
        case mbAnnotation of
          Just ann -> handleAnnotation pos ty ann
          Nothing -> do
            case ty of
              PARAGRAPH -> handleParagraph ign pos ty subs
              LINK {}   -> handleLink      ign pos ty subs
              IMAGE {}  -> handleLink      ign pos ty subs
              _         -> handleOther     ign pos ty subs

    handleLink ::
      Maybe Ignore ->
      Maybe PosInfo ->
      NodeType ->
      [ScannerM C.Node] ->
      ScannerM C.Node
    handleLink ign pos ty subs = do
      let traverseChildren = C.Node pos ty <$> sequence subs
      -- It can be checked that it's correct for all the cases
      ssIgnore .= Nothing

      case ign of
        Nothing -> traverseChildren
        Just (Ignore IMSParagraph modePos) -> do
          reportExpectedParagraphAfterIgnoreAnnotation modePos ty
          traverseChildren
        Just (Ignore (IMSLink _) _) -> do
          pure defNode

    handleParagraph ::
      Maybe Ignore ->
      Maybe PosInfo ->
      NodeType ->
      [ScannerM C.Node] ->
      ScannerM C.Node
    handleParagraph ign pos ty subs = do
      let traverseChildren = C.Node pos ty <$> sequence subs
      node <- case ign of
        Nothing -> traverseChildren
        Just (Ignore IMSParagraph _) -> do
          ssIgnore .= Nothing
          pure defNode
        Just (Ignore (IMSLink ignoreLinkState) modePos) ->
          traverseNodeWithLinkExpected ignoreLinkState modePos pos ty subs

      use ssIgnore >>= \case
        Just (Ignore (IMSLink ExpectingLinkInParagraph) pragmaPos) ->
          lift $ tell $ makeError pragmaPos fp LinkErr
        _ -> pass
      pure node

    handleOther ::
      Maybe Ignore ->
      Maybe PosInfo ->
      NodeType ->
      [ScannerM C.Node] ->
      ScannerM C.Node
    handleOther ign pos ty subs = do
      let traverseChildren = C.Node pos ty <$> sequence subs

      case ign of
        Nothing -> traverseChildren
        Just (Ignore IMSParagraph modePos) -> do
          reportExpectedParagraphAfterIgnoreAnnotation modePos ty
          ssIgnore .= Nothing
          traverseChildren
        Just (Ignore (IMSLink ignoreLinkState) modePos) -> do
          traverseNodeWithLinkExpected ignoreLinkState modePos pos ty subs

    reportExpectedParagraphAfterIgnoreAnnotation :: Maybe PosInfo -> NodeType -> ScannerM ()
    reportExpectedParagraphAfterIgnoreAnnotation modePos ty =
      lift . tell . makeError modePos fp . ParagraphErr $ prettyType ty

    traverseNodeWithLinkExpected ::
      IgnoreLinkState ->
      Maybe PosInfo ->
      Maybe PosInfo ->
      NodeType ->
      [ScannerM C.Node] ->
      ScannerM C.Node
    traverseNodeWithLinkExpected ignoreLinkState modePos pos ty subs = do
      when (ignoreLinkState == ExpectingLinkInSubnodes) $
        ssIgnore . _Just . ignoreMode .= IMSLink ParentExpectsLink
      node' <- C.Node pos ty <$> sequence subs
      when (ignoreLinkState == ExpectingLinkInSubnodes) $ do
        currentIgnore <- use ssIgnore
        case currentIgnore of
          Just (Ignore {_ignoreMode = IMSLink ParentExpectsLink}) -> do
            lift $ tell $ makeError modePos fp LinkErr
            ssIgnore .= Nothing
          _ -> pass
      return node'

    handleAnnotation
      :: Maybe PosInfo
      -> NodeType
      -> Annotation
      -> ScannerM C.Node
    handleAnnotation pos nodeType = \case
      IgnoreAnnotation mode -> do
        let reportIfThereWasAnnotation :: ScannerM ()
            reportIfThereWasAnnotation = do
              curIgnore <- use ssIgnore
              whenJust curIgnore $ \case
                Ignore IMSParagraph prevPos ->
                  lift . tell . makeError prevPos fp . ParagraphErr $ prettyType nodeType
                Ignore (IMSLink _) prevPos ->
                  lift $ tell $ makeError prevPos fp LinkErr

        mbIgnoreModeState <- case mode of
          IMLink -> do
            reportIfThereWasAnnotation
            use ssParentNodeType <&> Just . IMSLink . \case
              Just PARAGRAPH -> ExpectingLinkInParagraph
              _ -> ExpectingLinkInSubnodes

          IMParagraph -> do
            reportIfThereWasAnnotation
            pure $ Just IMSParagraph

          -- We don't expect to find an `ignore all` annotation here,
          -- since that annotation should be at the top of the file and
          -- the file should already be ignored when `checkIgnoreFile` is called.
          -- We should report an error if we find it anyway.
          IMAll -> do
            lift . tell $ makeError correctPos fp FileErr
            pure Nothing

        whenJust mbIgnoreModeState $ \ignoreModeState ->
          (ssIgnore .= Just (Ignore ignoreModeState correctPos))
        pure defNode
      InvalidAnnotation msg -> do
        lift . tell $ makeError correctPos fp $ UnrecognisedErr msg
        pure defNode
      where
        correctPos = getPosition $ C.Node pos nodeType []

    prettyType :: NodeType -> Text
    prettyType ty =
      let mType = safeHead $ words $ show ty
      in fromMaybe "" mType

    withIgnoreMode
      :: ScannerM C.Node
      -> Writer [ScanError] C.Node
    withIgnoreMode action = action `runStateT` initialScannerState >>= \case
      -- We expect `Ignore` state to be `Nothing` when we reach EOF,
      -- otherwise that means there was an annotation that didn't match
      -- any node, so we have to report that.
      (node, ScannerState {_ssIgnore = Just (Ignore mode pos)}) -> case mode of
        IMSParagraph -> do
            tell . makeError pos fp $ ParagraphErr "EOF"
            pure node
        IMSLink _ -> do
            tell $ makeError pos fp LinkErr
            pure node
      (node, _) -> pure node

-- | Custom `foldMap` for source tree.
foldNode :: (Monoid a, Monad m) => (C.Node -> m a) -> C.Node -> m a
foldNode action node@(C.Node _ _ subs) = do
  a <- action node
  b <- concatForM subs (foldNode action)
  return (a <> b)

type ExtractorM a = ReaderT MarkdownConfig (Writer [ScanError]) a

-- | Extract information from source tree.
nodeExtractInfo
  :: FilePath
  -> C.Node
  -> ExtractorM FileInfo
nodeExtractInfo fp (C.Node nPos nTy nSubs) = do
  let (ignoreFile, contentNodes) = checkGlobalAnnotations nSubs
  if ignoreFile
  then return def
  else diffToFileInfo <$>
       (lift (processAnnotations fp $ C.Node nPos nTy contentNodes)
         >>= foldNode extractor)

  where
    extractor :: C.Node -> ExtractorM FileInfoDiff
    extractor node@(C.Node pos ty _) =
      case ty of
        HTML_BLOCK _ -> do
          return mempty

        HEADING lvl -> do
          flavor <- asks mcFlavor
          let aType = HeaderAnchor lvl
          let aName = headerToAnchor flavor $ nodeExtractText node
          let aPos  = toPosition pos
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
                  aPos  = toPosition pos
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
         let rName = nodeExtractText node
             rPos = toPosition pos
             link = if null url then rName else url
         let (rLink, rAnchor) = case T.splitOn "#" link of
                 [t]    -> (t, Nothing)
                 t : ts -> (t, Just $ T.intercalate "#" ts)
                 []     -> error "impossible"
         return $ FileInfoDiff
           (DList.singleton $ Reference {rName, rPos, rLink, rAnchor})
           DList.empty

-- | Check for global annotations, ignoring simple comments if there are any.
checkGlobalAnnotations :: [C.Node] -> (Bool, [C.Node])
checkGlobalAnnotations nodes = do
  let (headerNodes, contentsNodes) = span isHeaderNode nodes
      ignoreFile = any isIgnoreFile headerNodes
  (ignoreFile, contentsNodes)
  where
    isSimpleComment :: C.Node -> Bool
    isSimpleComment node = do
      let isComment = isJust $ getCommentContent node
          isNotXrefcheckAnnotation = isNothing $ getXrefcheckContent node
      isComment && isNotXrefcheckAnnotation

    isIgnoreFile :: C.Node -> Bool
    isIgnoreFile = (Just (IgnoreAnnotation IMAll) ==) . getAnnotation

    isHeaderNode :: C.Node -> Bool
    isHeaderNode node =
      any ($ node)
        [ isSimpleComment
        , isIgnoreFile
        ]

defNode :: C.Node
defNode = C.Node Nothing DOCUMENT [] -- hard-coded default Node

makeError
  :: Maybe PosInfo
  -> FilePath
  -> ScanErrorDescription
  -> [ScanError]
makeError pos fp errDescription = one $ ScanError (toPosition pos) fp errDescription

getCommentContent :: C.Node -> Maybe Text
getCommentContent node = do
  txt <- getHTMLText node
  T.stripSuffix "-->" =<< T.stripPrefix "<!--" (T.strip txt)

getHTMLText :: C.Node -> Maybe Text
getHTMLText (C.Node _ (HTML_BLOCK txt) _) = Just txt
getHTMLText (C.Node _ (HTML_INLINE txt) _) = Just txt
getHTMLText _ = Nothing

getXrefcheckContent :: C.Node -> Maybe Text
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
getPosition :: C.Node -> Maybe PosInfo
getPosition node@(C.Node pos _ _) = do
  annLength <- length . T.strip <$> getHTMLText node
  PosInfo sl sc _ _ <- pos
  pure $ PosInfo sl sc sl (sc + annLength - 1)

-- | Extract `Annotation` if current node is xrefcheck annotation.
getAnnotation :: C.Node -> Maybe Annotation
getAnnotation node = getXrefcheckContent node <&> textToMode

textToMode :: Text -> Annotation
textToMode annText = case wordsList of
  ("ignore" : [x])
    | Just ignMode <- getIgnoreMode x -> IgnoreAnnotation ignMode
  _ -> InvalidAnnotation annText
  where
    wordsList = words annText

getIgnoreMode :: Text -> Maybe IgnoreMode
getIgnoreMode = \case
  "link" -> Just IMLink
  "paragraph" -> Just IMParagraph
  "all" -> Just IMAll
  _ -> Nothing

parseFileInfo :: MarkdownConfig -> FilePath -> LT.Text -> (FileInfo, [ScanError])
parseFileInfo config fp input
  = runWriter
  $ flip runReaderT config
  $ nodeExtractInfo fp
  $ commonmarkToNode [optFootnotes] [extAutolink]
  $ toStrict input

markdownScanner :: MarkdownConfig -> ScanAction
markdownScanner config path = parseFileInfo config path . decodeUtf8 <$> BSL.readFile path

markdownSupport :: MarkdownConfig -> ([Extension], ScanAction)
markdownSupport config = ([".md"], markdownScanner config)
