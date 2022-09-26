{- SPDX-FileCopyrightText: 2018-2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Markdown documents markdownScanner.

module Xrefcheck.Scanners.Markdown
  ( MarkdownConfig' (..)
  , MarkdownConfig
  , IgnoreMode (..)
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
import Fmt (Buildable (..), blockListF, nameF, (+|), (|+))
import Text.HTML.TagSoup

import Xrefcheck.Core
import Xrefcheck.Scan
import Xrefcheck.Util

-- | Type alias for MarkdownConfig' with all required fields.
type MarkdownConfig = MarkdownConfig' Identity

data MarkdownConfig' f = MarkdownConfig
  { mcFlavor :: Field f Flavor
  } deriving stock (Generic)

instance FromJSON (MarkdownConfig' Maybe) where
  parseJSON = genericParseJSON aesonConfigOption

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
  | None
  deriving stock (Eq)

-- | Bind `IgnoreMode` to its `PosInfo` so that we can tell where the
-- corresponding annotation was declared.
data Ignore = Ignore IgnoreMode (Maybe PosInfo)

type ScannerM a = StateT Ignore (Writer [ScanError]) a

-- | Empty `Ignore` state
ignoreNone :: Ignore
ignoreNone = Ignore None Nothing

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
      Ignore mode modePos <- get
      let node = Node pos ty []
      case (mode, ty) of
        -- We expect to find a paragraph immediately after the
        -- `ignore paragraph` annotanion. If the paragraph is not
        -- found we should report an error.
        (Paragraph, PARAGRAPH) -> put ignoreNone $> defNode
        (Paragraph, x)         -> do
          lift . tell . makeError modePos fp mode $ prettyType x
          put ignoreNone
          Node pos ty <$> sequence subs

        -- We don't expect to find an `ignore file` annotation here,
        -- since that annotation should be at the top of the file and
        -- the file should already be ignored when `checkIgnoreFile` is called.
        -- We should report an error if we find it anyway.
        (File, _)              -> do
          lift . tell $ makeError modePos fp mode ""
          put ignoreNone
          Node pos ty <$> sequence subs

        -- When we find an `ignore link` annotation, we skip nodes until
        -- we find a link and ignore it, or we find another ignore annotation,
        -- then we should report an error and set new `Ignore` state.
        (Link, LINK {})        -> put ignoreNone $> defNode
        (Link, _)            ->
          case getIgnoreMode node of
            Just mode'  -> do
              lift . tell $ makeError modePos fp mode ""
              handleMode node mode'
            Nothing     -> Node pos ty <$> sequence subs

        -- When no `Ignore` state is set check next node for annotation,
        -- if found then set it as new `IgnoreMode` otherwise skip node.
        (None, _)              ->
          case getIgnoreMode node of
            Just mode' -> handleMode node mode'
            Nothing    -> Node pos ty <$> sequence subs

    handleMode
      :: Node
      -> IgnoreMode
      -> ScannerM Node
    handleMode node = \case
      -- Report unknown `IgnoreMode`.
      None   -> do
        let unrecognised = fromMaybe ""
              $ safeHead . drop 1 . words =<< getXrefcheckContent node
        lift . tell $ makeError (getPosition node) fp None unrecognised
        put ignoreNone $> defNode
      -- Set new `Ignore` state.
      mode'  -> put (Ignore mode' $ getPosition node) $> defNode

    prettyType :: NodeType -> Text
    prettyType ty =
      let mType = safeHead $ words $ show ty
      in fromMaybe "" mType

    withIgnoreMode
      :: ScannerM Node
      -> Writer [ScanError] Node
    withIgnoreMode action = action `runStateT` ignoreNone >>= \case
      -- We expect `IgnoreMode` to be `None` when we reach EOF,
      -- otherwise that means there was an annotation that didn't match
      -- any node, so we have to report that.
      (node, Ignore None _) -> pure node
      (node, (Ignore mode pos))
        | mode == Paragraph -> do
            tell $ makeError pos fp mode "EOF"
            pure node
        -- Link and File scan errors do not require extra text info
        -- to make error description.
        | otherwise      -> do
            tell $ makeError pos fp mode ""
            pure node


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
    isIgnoreFile = (Just File ==) . getIgnoreMode

defNode :: Node
defNode = Node Nothing DOCUMENT [] -- hard-coded default Node

makeError
  :: Maybe PosInfo
  -> FilePath
  -> IgnoreMode
  -> Text
  -> [ScanError]
makeError pos fp mode txt = one . ScanError (toPosition pos) fp $ case mode of
  Link      -> linkMsg
  Paragraph -> paragraphMsg
  File      -> fileMsg
  None      -> unrecognisedMsg
  where
    fileMsg :: Text
    fileMsg =
      "Annotation \"ignore file\" must be at the top of \
      \markdown or right after comments at the top"

    linkMsg :: Text
    linkMsg = "Expected a LINK after \"ignore link\" annotation"

    paragraphMsg :: Text
    paragraphMsg = unwords
      [ "Expected a PARAGRAPH after \
          \\"ignore paragraph\" annotation, but found"
      , txt
      ]

    unrecognisedMsg :: Text
    unrecognisedMsg = unwords
      [ "Unrecognised option"
      , "\"" <> txt <> "\""
      , "perhaps you meant \
          \<\"ignore link\"|\"ignore paragraph\"|\"ignore file\"> "
      ]

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
getIgnoreMode :: Node -> Maybe IgnoreMode
getIgnoreMode node = textToMode . words =<< getXrefcheckContent node

textToMode :: [Text] -> Maybe IgnoreMode
textToMode ("ignore" : [x])
  | x == "link"      = return Link
  | x == "paragraph" = return Paragraph
  | x == "file"      = return File
  | otherwise        = return None
textToMode _         = Nothing

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
