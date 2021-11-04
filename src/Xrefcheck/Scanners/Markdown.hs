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
  ) where

import Universum

import CMarkGFM (Node (..), NodeType (..), PosInfo (..), commonmarkToNode)
import Control.Monad.Except (MonadError, throwError)
import Data.Aeson.TH (deriveFromJSON)
import Data.ByteString.Lazy qualified as BSL
import Data.DList qualified as DList
import Data.Default (def)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Fmt (Buildable (..), blockListF, nameF, (+|), (|+))

import Xrefcheck.Core
import Xrefcheck.Scan
import Xrefcheck.Util

data MarkdownConfig = MarkdownConfig
  { mcFlavor :: Flavor
  }

deriveFromJSON aesonConfigOption ''MarkdownConfig

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
        " " +|
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

-- | A fold over a `Node`.
cataNode :: (Maybe PosInfo -> NodeType -> [c] -> c) -> Node -> c
cataNode f (Node pos ty subs) = f pos ty (cataNode f <$> subs)

-- | Remove nodes with accordance with global `MarkdownConfig` and local
--   overrides.
removeIgnored :: Node -> Either Text Node
removeIgnored = runIdentity . runExceptT . flip evalStateT None . cataNode remove
  where
    remove
      :: (MonadError Text m, MonadState IgnoreMode m)
      => Maybe PosInfo
      -> NodeType
      -> [m Node]
      -> m Node
    remove pos ty subs = do
      mode <- get
      case (mode, ty) of
        (Paragraph, PARAGRAPH) -> put None $> defNode
        (Paragraph, x)         -> throwError (makeError mode (prettyType x) pos)
        (File, _)              -> throwError (makeError mode "" pos)
        (Link, LINK {})        -> put None $> defNode
        (Link, _)              -> Node pos ty <$> sequence subs
        (None, _) -> do
          case getIgnoreMode (Node pos ty []) of
            Just mode' -> put mode' $> defNode
            Nothing    -> Node pos ty <$> sequence subs

    prettyType :: NodeType -> Text
    prettyType ty =
      let mType = safeHead $ words $ show ty
      in fromMaybe "" mType

-- | Custom `foldMap` for source tree.
foldNode :: (Monoid a, Monad m) => (Node -> m a) -> Node -> m a
foldNode action node@(Node _ _ subs) = do
  a <- action node
  b <- concatForM subs (foldNode action)
  return (a <> b)

-- | Extract information from source tree.
nodeExtractInfo
  :: forall m
  .  ( MonadError Text m
     , MonadState IgnoreMode m
     , MonadReader MarkdownConfig m
     )
  => Node
  -> m FileInfo
nodeExtractInfo input@(Node _ _ nSubs) = do
  if checkIgnoreFile nSubs
  then return def
  else case removeIgnored input of
    Left err -> throwError err
    Right relevant ->
      diffToFileInfo <$> foldNode extractor relevant

  where
    extractor :: Node -> m FileInfoDiff
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
          let mName = T.stripSuffix "\">" =<< T.stripPrefix "<a name=\"" text
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
  :: IgnoreMode
  -> Text
  -> Maybe PosInfo
  -> Text
makeError mode txt pos =
  let errMsg = case mode of
        Link      -> linkMsg
        Paragraph -> paragraphMsg
        File      -> fileMsg
        None      -> unrecognisedMsg
  in errMsg <> posInfo
  where
    posInfo :: Text
    posInfo =
      let posToText :: Position -> Text
          posToText (Position mPos) = fromMaybe "" mPos
      in "(" <> posToText (toPosition pos) <> ")"

    fileMsg :: Text
    fileMsg =
      "\"ignore file\" must be at the top of \
      \markdown or right after comments at the top"

    linkMsg :: Text
    linkMsg = "expected a LINK after \"ignore link\" "

    paragraphMsg :: Text
    paragraphMsg = unwords
      [ "expected a PARAGRAPH after \
          \\"ignore paragraph\", but found"
      , txt
      , ""
      ]

    unrecognisedMsg :: Text
    unrecognisedMsg = unwords
      [ "unrecognised option"
      , "\"" <> txt <> "\""
      , "perhaps you meant \
          \<\"ignore link\"|\"ignore paragraph\"|\"ignore file\"> "
      ]

getCommentContent :: Node -> Maybe Text
getCommentContent node = do
  txt <- getHTMLText node
  T.stripSuffix "-->" =<< T.stripPrefix "<!--" (T.strip txt)
  where
    getHTMLText :: Node -> Maybe Text
    getHTMLText (Node _ (HTML_BLOCK txt) _) = Just txt
    getHTMLText (Node _ (HTML_INLINE txt) _) = Just txt
    getHTMLText _ = Nothing

getXrefcheckContent :: Node -> Maybe Text
getXrefcheckContent node =
  let notStripped = T.stripPrefix "xrefcheck:" . T.strip =<<
        getCommentContent node
  in T.strip <$> notStripped

getIgnoreMode :: Node -> Maybe IgnoreMode
getIgnoreMode node =
  let mContent = getXrefcheckContent node

  in textToMode . words =<< mContent

textToMode :: [Text] -> Maybe IgnoreMode
textToMode ("ignore" : [x])
  | x == "link"      = return Link
  | x == "paragraph" = return Paragraph
  | x == "file"      = return File
  | otherwise        = return None
textToMode _         = Nothing

parseFileInfo :: MarkdownConfig -> LT.Text -> Either Text FileInfo
parseFileInfo config input
  = runIdentity
  $ runExceptT
  $ flip runReaderT config
  $ flip evalStateT None
  $ nodeExtractInfo
  $ commonmarkToNode [] []
  $ toStrict input

markdownScanner :: MarkdownConfig -> ScanAction
markdownScanner config path = do
  errOrInfo <- parseFileInfo config . decodeUtf8 <$> BSL.readFile path
  case errOrInfo of
    Left errTxt -> do
      die $ "Error when scanning " <> path <> ": " <> T.unpack errTxt
    Right fileInfo -> return fileInfo

markdownSupport :: MarkdownConfig -> ([Extension], ScanAction)
markdownSupport config = ([".md"], markdownScanner config)
