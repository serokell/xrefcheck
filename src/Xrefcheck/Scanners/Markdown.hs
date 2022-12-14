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

data Node a = Node
  { _ndPos :: Maybe PosInfo
  , _ndType :: NodeType
  , _ndInfo :: a
  , _ndSubs :: [Node a]
  }

instance Buildable (Node a) where
  build (Node _mpos ty _info mSubs) = nameF (show ty) $
    maybe "[]" interpolateBlockListF (nonEmpty mSubs)

-- Here and below CPC stands for "copy/paste check"
type NodeCPC = Node CopyPasteCheck

newtype CopyPasteCheck = CopyPasteCheck
  { cpcShouldCheck :: Bool
  } deriving stock (Show, Eq, Generic)

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
nodeExtractText :: Node info -> Text
nodeExtractText = T.strip . mconcat . map extractText . nodeFlatten
  where
    extractText = \case
      TEXT t -> t
      CODE t -> t
      _ -> ""

    nodeFlatten :: Node info -> [NodeType]
    nodeFlatten (Node _pos ty _info subs) = ty : concatMap nodeFlatten subs


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

data GetAnnotation
  = IgnoreAnnotation IgnoreMode
  | IgnoreCopyPasteCheck IgnoreMode
  | InvalidAnnotation Text
  deriving stock (Eq)



data ScannerState = ScannerState
  { _ssIgnore :: Maybe Ignore
  , _ssIgnoreCopyPasteCheck :: Maybe Ignore
  , _ssParagraphExpectedAfterCpcAnnotation :: Bool
  , _ssParentNodeType :: Maybe NodeType
  -- ^ @cataNodeWithParentNodeInfo@ allows to get a @NodeType@ of parent node from this field
  }
makeLenses ''ScannerState

initialScannerState :: ScannerState
initialScannerState = ScannerState
  { _ssIgnore = Nothing
  , _ssIgnoreCopyPasteCheck = Nothing
  , _ssParentNodeType = Nothing
  , _ssParagraphExpectedAfterCpcAnnotation = False
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
-- and remove nodes that should be ignored;
-- find copy/paste check annotations (ignore for paragraph and for link)
-- and label nodes with a boolean meaning whether they should be
-- copy/paste checked.
processAnnotations :: FilePath -> C.Node -> Writer [ScanError] NodeCPC
processAnnotations fp = withIgnoreMode . cataNodeWithParentNodeInfo process
  where
    process
      :: Maybe PosInfo
      -> NodeType
      -> [ScannerM NodeCPC]
      -> ScannerM NodeCPC
    process pos ty subs = do
      let node = C.Node pos ty []
      use ssIgnore >>= \ign ->
        use ssIgnoreCopyPasteCheck >>= \ignCPC -> do
        -- When no `Ignore` state is set check next node for annotation,
        -- if found then set it as new `IgnoreMode` otherwise skip node.
        case getAnnotation node of
          Just ann -> handleAnnotation pos ty ann
          Nothing -> do
            case ty of
              PARAGRAPH -> handleParagraph ign ignCPC pos ty subs
              LINK {}   -> handleLink      ign ignCPC pos ty subs
              IMAGE {}  -> handleLink      ign ignCPC pos ty subs
              _         -> handleOther     ign ignCPC pos ty subs

    handleLink ::
      Maybe Ignore ->
      Maybe Ignore ->
      Maybe PosInfo ->
      NodeType ->
      [ScannerM NodeCPC] ->
      ScannerM NodeCPC
    handleLink ign ignCPC pos ty subs = do
      let shouldCheckCPC = CopyPasteCheck $ isNothing ignCPC
      let traverseChildren = Node pos ty shouldCheckCPC <$> sequence subs
      -- It's common for all ignore states
      ssIgnore .= Nothing
      -- If there was a copy/paste ignore annotation that expected link,
      -- reset this state
      resetCpcIgnoreIfLink
      -- If right now there was a copy/paste ignore annotation for paragraph,
      -- emit an error and reset these states.
      reportExpectedParagraphAfterIgnoreCpcAnnotation ty

      case ign of
        Nothing -> traverseChildren
        Just (Ignore IMSParagraph modePos) -> do
          reportExpectedParagraphAfterIgnoreAnnotation modePos ty
          traverseChildren
        Just (Ignore (IMSLink _) _) -> do
          pure defNode

    handleParagraph ::
      Maybe Ignore ->
      Maybe Ignore ->
      Maybe PosInfo ->
      NodeType ->
      [ScannerM NodeCPC] ->
      ScannerM NodeCPC
    handleParagraph ign ignCPC pos ty subs = do
      let shouldCheckCPC = CopyPasteCheck $ isNothing ignCPC
      let traverseChildren = Node pos ty shouldCheckCPC <$> sequence subs
      -- If a new paragraph was expected (this stands for True), now we
      -- don't expect paragraphs any more.
      ssParagraphExpectedAfterCpcAnnotation .= False
      node <- case ign of
        Nothing ->
          wrapTraverseNodeWithLinkExpectedForCpc traverseChildren
        Just (Ignore IMSParagraph _) -> do
          ssIgnore .= Nothing
          pure defNode
        Just (Ignore (IMSLink ignoreLinkState) modePos) ->
          wrapTraverseNodeWithLinkExpected ignoreLinkState modePos $
          wrapTraverseNodeWithLinkExpectedForCpc traverseChildren

      ssIgnoreCopyPasteCheck .= Nothing

      use ssIgnore >>= \case
        Just (Ignore (IMSLink ExpectingLinkInParagraph) pragmaPos) -> do
          lift $ tell $ makeError pragmaPos fp LinkErr
          ssIgnore .= Nothing
        _ -> pass
      use ssIgnoreCopyPasteCheck >>= \case
        Just (Ignore (IMSLink ExpectingLinkInParagraph) pragmaPos) -> do
          lift $ tell $ makeError pragmaPos fp LinkErrCpc
          ssIgnoreCopyPasteCheck .= Nothing
        _ -> pass

      pure node

    handleOther ::
      Maybe Ignore ->
      Maybe Ignore ->
      Maybe PosInfo ->
      NodeType ->
      [ScannerM NodeCPC] ->
      ScannerM NodeCPC
    handleOther ign ignCPC pos ty subs = do
      let shouldCheckCPC = CopyPasteCheck $ isNothing ignCPC
      let traverseChildren = Node pos ty shouldCheckCPC <$> sequence subs
      -- If right now there was a copy/paste ignore annotation for paragraph,
      -- emit an error and reset these states.
      reportExpectedParagraphAfterIgnoreCpcAnnotation ty

      case ign of
        Nothing ->
          wrapTraverseNodeWithLinkExpectedForCpc traverseChildren
        Just (Ignore IMSParagraph modePos) -> do
          reportExpectedParagraphAfterIgnoreAnnotation modePos ty
          ssIgnore .= Nothing
          wrapTraverseNodeWithLinkExpectedForCpc traverseChildren
        Just (Ignore (IMSLink ignoreLinkState) modePos) ->
          wrapTraverseNodeWithLinkExpected ignoreLinkState modePos $
          wrapTraverseNodeWithLinkExpectedForCpc traverseChildren

    reportExpectedParagraphAfterIgnoreAnnotation :: Maybe PosInfo -> NodeType -> ScannerM ()
    reportExpectedParagraphAfterIgnoreAnnotation modePos ty =
      lift . tell . makeError modePos fp . ParagraphErr $ prettyType ty

    resetCpcIgnoreIfLink :: ScannerM ()
    resetCpcIgnoreIfLink = do
      curCpcIgnore <- use ssIgnoreCopyPasteCheck
      case _ignoreMode <$> curCpcIgnore of
        Just (IMSLink _) -> ssIgnoreCopyPasteCheck .= Nothing
        _ -> pass

    reportExpectedParagraphAfterIgnoreCpcAnnotation ::
      NodeType -> ScannerM ()
    reportExpectedParagraphAfterIgnoreCpcAnnotation ty =
      use ssIgnoreCopyPasteCheck >>= \case
        Just (Ignore IMSParagraph modePos) ->
          whenM (use ssParagraphExpectedAfterCpcAnnotation) $ do
            lift . tell . makeError modePos fp . ParagraphErrCpc $ prettyType ty
            ssParagraphExpectedAfterCpcAnnotation .= False
            ssIgnoreCopyPasteCheck .= Nothing
        _ -> pass

    wrapTraverseNodeWithLinkExpected ::
      IgnoreLinkState ->
      Maybe PosInfo ->
      ScannerM NodeCPC ->
      ScannerM NodeCPC
    wrapTraverseNodeWithLinkExpected ignoreLinkState modePos =
      if ignoreLinkState /= ExpectingLinkInSubnodes
      then id
      else \traverse' -> do
        ssIgnore . _Just . ignoreMode .=  IMSLink ParentExpectsLink
        node' <- traverse'
        currentIgnore <- use ssIgnore
        case currentIgnore of
          Just (Ignore {_ignoreMode = IMSLink ParentExpectsLink}) -> do
            lift $ tell $ makeError modePos fp LinkErr
            ssIgnore .= Nothing
          _ -> pass
        return node'

    wrapTraverseNodeWithLinkExpectedForCpc ::
      ScannerM NodeCPC ->
      ScannerM NodeCPC
    wrapTraverseNodeWithLinkExpectedForCpc traverse' = do
      ignoreCpc <- use ssIgnoreCopyPasteCheck
      case ignoreCpc of
        Just (Ignore (IMSLink ExpectingLinkInSubnodes) modePos) -> do
          ssIgnoreCopyPasteCheck . _Just . ignoreMode .=  IMSLink ParentExpectsLink
          node' <- traverse'
          currentIgnore <- use ssIgnoreCopyPasteCheck
          case currentIgnore of
            Just (Ignore {_ignoreMode = IMSLink ParentExpectsLink}) -> do
              lift $ tell $ makeError modePos fp LinkErrCpc
              ssIgnoreCopyPasteCheck .= Nothing
            _ -> pass
          return node'
        _ -> traverse'

    handleAnnotation
      :: Maybe PosInfo
      -> NodeType
      -> GetAnnotation
      -> ScannerM NodeCPC
    handleAnnotation pos nodeType = \case
      IgnoreAnnotation mode  -> do
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
      IgnoreCopyPasteCheck mode -> do
        mbIgnoreModeState <- case mode of
          IMLink -> use ssParentNodeType <&> Just . IMSLink . \case
             Just PARAGRAPH -> ExpectingLinkInParagraph
             _ -> ExpectingLinkInSubnodes

          IMParagraph -> do
            ssParagraphExpectedAfterCpcAnnotation .= True
            pure $ Just IMSParagraph

          -- We don't expect to find an `ignore all` annotation here,
          -- since that annotation should be at the top of the file and
          -- any correct annotations should be handled in `checkGlobalAnnotations`
          -- function.
          IMAll -> do
            lift . tell $ makeError correctPos fp FileErrCpc
            pure Nothing

        whenJust mbIgnoreModeState $ \ignoreModeState -> do
          let setupNewCpcState = ssIgnoreCopyPasteCheck .= Just (Ignore ignoreModeState correctPos)
          use ssIgnoreCopyPasteCheck >>= \case
            Nothing -> setupNewCpcState
            Just (Ignore curIgn prevPos)
              | IMSLink _ <- curIgn -> do
              lift $ tell $ makeError prevPos fp LinkErrCpc
              setupNewCpcState
              | IMSParagraph <- curIgn -> case ignoreModeState of
                  IMSParagraph -> do
                    lift . tell . makeError prevPos fp . ParagraphErrCpc $ prettyType nodeType
                    setupNewCpcState
                  -- It's OK to have link annotation when paragraph is ignored
                  -- because in this case all links and all annotations are ignored.
                  _ -> pass
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
      :: ScannerM (Node info)
      -> Writer [ScanError] (Node info)
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
foldNode :: (Monoid a, Monad m) => (Node info -> m a) -> Node info -> m a
foldNode action node@(Node _ _ _ subs) = do
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
  let (ignoreFile, ignoreCpcInFile, contentNodes) = checkGlobalAnnotations nSubs
  if ignoreFile
  then return def
  else diffToFileInfo (not ignoreCpcInFile) <$>
       (lift (processAnnotations fp $ C.Node nPos nTy contentNodes)
         >>= foldNode extractor)

  where
    extractor :: NodeCPC -> ExtractorM FileInfoDiff
    extractor node@(Node pos ty info _) =
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
           (DList.singleton $
              Reference {rName, rPos, rLink, rAnchor, rCheckCopyPaste = cpcShouldCheck info})
           DList.empty

-- | Check for global annotations, ignoring simple comments if there are any.
checkGlobalAnnotations :: [C.Node] -> (Bool, Bool, [C.Node])
checkGlobalAnnotations nodes = do
  let (headerNodes, contentsNodes) = span isHeaderNode nodes
      ignoreFile = any isIgnoreFile headerNodes
      ignoreCpcInFile = any isIgnoreCpcWithinFile headerNodes
  (ignoreFile, ignoreCpcInFile, contentsNodes)
  where
    isSimpleComment :: C.Node -> Bool
    isSimpleComment node = do
      let isComment = isJust $ getCommentContent node
          isNotXrefcheckAnnotation = isNothing $ getXrefcheckContent node
      isComment && isNotXrefcheckAnnotation

    isIgnoreFile :: C.Node -> Bool
    isIgnoreFile = (Just (IgnoreAnnotation IMAll) ==) . getAnnotation

    isIgnoreCpcWithinFile :: C.Node -> Bool
    isIgnoreCpcWithinFile = (Just (IgnoreCopyPasteCheck IMAll) ==) . getAnnotation

    isHeaderNode :: C.Node -> Bool
    isHeaderNode node =
      any ($ node)
        [ isSimpleComment
        , isIgnoreFile
        , isIgnoreCpcWithinFile
        ]

-- | Hard-coded default Node
defNode :: NodeCPC
defNode = Node Nothing DOCUMENT (CopyPasteCheck False) []

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

-- | Extract `IgnoreMode` if current node is xrefcheck annotation.
getAnnotation :: C.Node -> Maybe GetAnnotation
getAnnotation node = getXrefcheckContent node <&> textToMode

textToMode :: Text -> GetAnnotation
textToMode annText = case wordsList of
  ("ignore" : [x])
    | Just ignMode <- getIgnoreMode x -> IgnoreAnnotation ignMode
  ("no" : "duplication" : "check" : "in" : [x])
    | Just ignMode <- getIgnoreMode x -> IgnoreCopyPasteCheck ignMode
  _ -> InvalidAnnotation annText
  where
    wordsList = words annText

getIgnoreMode :: Text -> Maybe IgnoreMode
getIgnoreMode = \case
  "link" -> Just IMLink
  "paragraph" -> Just IMParagraph
  "all" -> Just IMAll
  "file" -> Just IMAll
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
