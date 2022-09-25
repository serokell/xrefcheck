{- SPDX-FileCopyrightText: 2018-2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Markdown documents markdownScanner.

module Xrefcheck.Scanners.Markdown
  ( MarkdownConfig (..)
  , ModeErr (..)
  , defGithubMdConfig
  , markdownScanner
  , markdownSupport
  , parseFileInfo
  , makeError
  ) where

import Universum

import CMarkGFM (Node (..), NodeType (..), PosInfo (..), commonmarkToNode, optFootnotes)
import Control.Monad.Trans.Writer.CPS (Writer, runWriter, tell)
import Data.Aeson (FromJSON (..), genericParseJSON)
import Data.ByteString.Lazy qualified as BSL
import Data.DList qualified as DList
import Data.Default (def)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder (toLazyText)
import Fmt (Buildable (..), blockListF, nameF, (+|), (|+))
import Text.HTML.TagSoup

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
  build (Node _mpos ty subs) = nameF (show ty) $ blockListF subs

toPosition :: Maybe PosInfo -> Position
toPosition = Position . \case
  Nothing -> Nothing
  Just PosInfo{..}
    | startLine == endLine -> Just $
        startLine |+ ":" +| startColumn |+ "-" +| endColumn |+ ""
    | otherwise -> Just $
        "" +|
        startLine |+ ":" +| startColumn |+ " - " +|
        endLine |+ ":" +| endColumn |+ ""

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
  = Link
  | Paragraph
  | File
  deriving stock (Eq)

-- | Bind `IgnoreMode` to its `PosInfo` so that we can tell where the
-- corresponding annotation was declared.
data Ignore = Ignore IgnoreMode (Maybe PosInfo)

data GetIgnoreMode
  = NotAnAnnotation
  | ValidMode IgnoreMode
  | InvalidMode Text
  deriving stock (Eq)

data ModeErr
  = LinkErr
  | FileErr
  | ParagraphErr Text
  | UnrecognisedErr Text

instance Buildable ModeErr where
  build = \case
    LinkErr -> "Expected a LINK after \"ignore link\" annotation"
    FileErr -> "Annotation \"ignore file\" must be at the top of \
      \markdown or right after comments at the top"
    ParagraphErr txt -> "Expected a PARAGRAPH after \
          \\"ignore paragraph\" annotation, but found " +| txt |+ ""
    UnrecognisedErr txt ->  "Unrecognised option \"" +| txt |+ "\" perhaps you meant \
          \<\"ignore link\"|\"ignore paragraph\"|\"ignore file\"> "

type ScannerM a = StateT (Maybe Ignore) (Writer [ScanError]) a

-- | A fold over a `Node`.
cataNode :: (Maybe PosInfo -> NodeType -> [c] -> c) -> Node -> c
cataNode f (Node pos ty subs) = f pos ty (cataNode f <$> subs)

-- | Remove nodes with accordance with global `MarkdownConfig` and local
--   overrides.
removeIgnored :: FilePath -> Node -> Writer [ScanError] Node
removeIgnored fp = withIgnoreMode . cataNode remove
  where
    remove
      :: Maybe PosInfo
      -> NodeType
      -> [ScannerM Node]
      -> ScannerM Node
    remove pos ty subs = do
      let node = Node pos ty []
      get >>= \case
        -- When no `Ignore` state is set check next node for annotation,
        -- if found then set it as new `IgnoreMode` otherwise skip node.
        Nothing                    -> handleIgnoreMode pos ty subs $ getIgnoreMode node
        Just (Ignore mode modePos) ->
          case (mode, ty) of
            -- We expect to find a paragraph immediately after the
            -- `ignore paragraph` annotanion. If the paragraph is not
            -- found we should report an error.
            (Paragraph, PARAGRAPH) -> put Nothing $> defNode
            (Paragraph, x)         -> do
              lift . tell . makeError modePos fp . ParagraphErr $ prettyType x
              put Nothing
              Node pos ty <$> sequence subs

            -- We don't expect to find an `ignore file` annotation here,
            -- since that annotation should be at the top of the file and
            -- the file should already be ignored when `checkIgnoreFile` is called.
            -- We should report an error if we find it anyway.
            (File, _)              -> do
              lift . tell $ makeError modePos fp FileErr
              put Nothing
              Node pos ty <$> sequence subs

            -- When we find an `ignore link` annotation, we skip nodes until
            -- we find a link and ignore it, or we find another ignore annotation,
            -- then we should report an error and set new `Ignore` state.
            (Link, LINK {})        -> put Nothing $> defNode
            (Link, _)              -> do
              let ignoreMode = getIgnoreMode node
              unless (ignoreMode == NotAnAnnotation) $
                lift . tell $ makeError modePos fp LinkErr
              handleIgnoreMode pos ty subs ignoreMode

    handleIgnoreMode
      :: Maybe PosInfo
      -> NodeType
      -> [ScannerM Node]
      -> GetIgnoreMode
      -> ScannerM Node
    handleIgnoreMode pos mode subs = \case
      ValidMode mode' ->
        put (Just . Ignore mode' $ getPosition node) $> defNode
      InvalidMode msg -> do
        lift . tell $ makeError (getPosition node) fp $ UnrecognisedErr msg
        put Nothing $> defNode
      NotAnAnnotation -> Node pos mode <$> sequence subs
      where
        node = Node pos mode []

    prettyType :: NodeType -> Text
    prettyType ty =
      let mType = safeHead $ words $ show ty
      in fromMaybe "" mType

    withIgnoreMode
      :: ScannerM Node
      -> Writer [ScanError] Node
    withIgnoreMode action = action `runStateT` Nothing >>= \case
      -- We expect `Ignore` state to be `Nothing` when we reach EOF,
      -- otherwise that means there was an annotation that didn't match
      -- any node, so we have to report that.
      (node, Just (Ignore mode pos))
        | mode == Paragraph -> do
            tell . makeError pos fp $ ParagraphErr "EOF"
            pure node
        | mode == Link -> do
            tell $ makeError pos fp LinkErr
            pure node
        | mode == File -> do
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
  if checkIgnoreFile nSubs
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

        LINK url _ -> do
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

        _ -> return mempty

-- | Check if there is `ignore file` at the beginning of the file,
-- ignoring preceding comments if there are any.
checkIgnoreFile :: [Node] -> Bool
checkIgnoreFile nodes =
  let isSimpleComment :: Node -> Bool
      isSimpleComment node = isComment node && not (isIgnoreFile node)

      mIgnoreFile = safeHead $ dropWhile isSimpleComment nodes
  in maybe False isIgnoreFile mIgnoreFile
  where
    isComment :: Node -> Bool
    isComment = isJust . getCommentContent

    isIgnoreFile :: Node -> Bool
    isIgnoreFile = (ValidMode File ==) . getIgnoreMode

defNode :: Node
defNode = Node Nothing DOCUMENT [] -- hard-coded default Node

makeError
  :: Maybe PosInfo
  -> FilePath
  -> ModeErr
  -> [ScanError]
makeError pos fp modeErr = one
  . ScanError (toPosition pos) fp
  . toStrict . toLazyText $ build modeErr

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
  | x == "link"      = ValidMode Link
  | x == "paragraph" = ValidMode Paragraph
  | x == "file"      = ValidMode File
  | otherwise        = InvalidMode x
textToMode _         = NotAnAnnotation

parseFileInfo :: MarkdownConfig -> FilePath -> LT.Text -> (FileInfo, [ScanError])
parseFileInfo config fp input
  = runWriter
  $ flip runReaderT config
  $ nodeExtractInfo fp
  $ commonmarkToNode [optFootnotes] []
  $ toStrict input

markdownScanner :: MarkdownConfig -> ScanAction
markdownScanner config path = parseFileInfo config path . decodeUtf8 <$> BSL.readFile path

markdownSupport :: MarkdownConfig -> ([Extension], ScanAction)
markdownSupport config = ([".md"], markdownScanner config)
