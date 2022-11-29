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

import CMarkGFM
  (Node (..), NodeType (..), PosInfo (..), commonmarkToNode, extAutolink, optFootnotes)
import Control.Lens (_Just, makeLenses, makeLensesFor, (.=))
import Control.Monad.Trans.Writer.CPS (Writer, runWriter, tell)
import Data.Aeson (FromJSON (..), genericParseJSON)
import Data.ByteString.Lazy qualified as BSL
import Data.DList qualified as DList
import Data.Default (def)
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

instance Buildable Node where
  build (Node _mpos ty mSubs) = nameF (show ty) $
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

type ScannerM a = StateT ScannerState (Writer [ScanError]) a

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
-- and remove nodes that should be ignored
removeIgnored :: FilePath -> Node -> Writer [ScanError] Node
removeIgnored fp = withIgnoreMode . cataNodeWithParentNodeInfo remove
  where
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
        Nothing                    -> handleIgnoreMode pos ty subs $ getIgnoreMode node
        Just (Ignore mode modePos) ->
          case (mode, ty) of
            -- We expect to find a paragraph immediately after the
            -- `ignore paragraph` annotanion. If the paragraph is not
            -- found we should report an error.
            (IMSParagraph, PARAGRAPH)    -> (ssIgnore .= Nothing) $> defNode
            (IMSParagraph, x)            -> do
              lift . tell . makeError modePos fp . ParagraphErr $ prettyType x
              ssIgnore .= Nothing
              Node pos ty <$> sequence subs

            -- We don't expect to find an `ignore all` annotation here,
            -- since that annotation should be at the top of the file and
            -- the file should already be ignored when `checkIgnoreFile` is called.
            -- We should report an error if we find it anyway.
            (IMSAll, _)                 -> do
              lift . tell $ makeError modePos fp FileErr
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
                    lift $ tell $ makeError modePos fp LinkErr
                    ssIgnore .= Nothing
                  _ -> pass
              return node'

      when (ty == PARAGRAPH) $ use ssIgnore >>= \case
        Just (Ignore (IMSLink ExpectingLinkInParagraph) pragmaPos) ->
          lift $ tell $ makeError pragmaPos fp LinkErr
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
        lift . tell $ makeError correctPos fp $ UnrecognisedErr msg
        (ssIgnore .= Nothing) $> defNode
      NotAnAnnotation -> Node pos nodeType <$> sequence subs
      where
        correctPos = getPosition $ Node pos nodeType []

    prettyType :: NodeType -> Text
    prettyType ty =
      let mType = safeHead $ words $ show ty
      in fromMaybe "" mType

    withIgnoreMode
      :: ScannerM Node
      -> Writer [ScanError] Node
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
        IMSAll -> do
            tell $ makeError pos fp FileErr
            pure node
      (node, _) -> pure node

-- | Custom `foldMap` for source tree.
foldNode :: (Monoid a, Monad m) => (Node -> m a) -> Node -> m a
foldNode action node@(Node _ _ subs) = do
  a <- action node
  b <- concatForM subs (foldNode action)
  return (a <> b)

type ExtractorM a = ReaderT MarkdownConfig (Writer [ScanError]) a

-- | Extract information from source tree.
nodeExtractInfo
  :: FilePath
  -> Node
  -> ExtractorM FileInfo
nodeExtractInfo fp input@(Node _ _ nSubs) = do
  if checkIgnoreAllFile nSubs
  then return def
  else diffToFileInfo <$> (foldNode extractor =<< lift (removeIgnored fp input))

  where
    extractor :: Node -> ExtractorM FileInfoDiff
    extractor node@(Node pos ty _) =
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
  :: Maybe PosInfo
  -> FilePath
  -> ScanErrorDescription
  -> [ScanError]
makeError pos fp errDescription = one $ ScanError (toPosition pos) fp errDescription

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
