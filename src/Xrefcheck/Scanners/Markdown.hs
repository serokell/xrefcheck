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

import CMarkGFM (Node (..), NodeType (..), PosInfo (..), commonmarkToNode)
import Control.Lens ((%=))
import Control.Monad.Trans.Except (Except, runExcept, throwE)
import Data.Aeson.TH (deriveFromJSON)
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isSpace)
import Data.Default (Default (..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
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

nodeFlatten :: Node -> [NodeType]
nodeFlatten (Node _pos ty subs) = ty : concatMap nodeFlatten subs

nodeExtractText :: Node -> Text
nodeExtractText = T.strip . mconcat . map extractText . nodeFlatten
  where
    extractText = \case
      TEXT t -> t
      CODE t -> t
      _ -> ""

data IgnoreMode
  = Link
  | Paragraph
  | File
  deriving Eq

nodeExtractInfo :: MarkdownConfig -> Node -> Except Text FileInfo
nodeExtractInfo config (Node _ _ docNodes) =
  if checkIgnoreFile docNodes
  then return def
  else finaliseFileInfo <$> extractionResult
  where
    extractionResult :: Except Text FileInfo
    extractionResult = execStateT (loop docNodes Nothing) def

    loop :: [Node] -> Maybe IgnoreMode -> StateT FileInfo (Except Text) ()
    loop [] _ = pass
    loop (node@(Node pos ty subs) : nodes) toIgnore
      | toIgnore == Just File = returnError toIgnore "" pos
      | toIgnore == Just Link = do
          let (Node startPos _ _) = maybe defNode id $ safeHead subs
          let mNext = case ty of
                PARAGRAPH -> afterIgnoredLink subs <> Just nodes
                TEXT txt | null (dropWhile isSpace $ T.unpack txt) -> afterIgnoredLink nodes
                SOFTBREAK -> afterIgnoredLink nodes
                _ -> afterIgnoredLink (node : nodes)
          case mNext of
            Just next -> loop next Nothing
            Nothing   -> returnError toIgnore "" startPos
      | toIgnore == Just Paragraph =
          case ty of
            PARAGRAPH -> loop nodes Nothing
            _         -> returnError toIgnore (prettyType ty) pos
      | otherwise =
          case ty of
            HTML_BLOCK _ -> processHtmlNode node pos nodes toIgnore
            HEADING lvl -> do
              let aType = HeaderAnchor lvl
              let aName = headerToAnchor (mcFlavor config) $
                          nodeExtractText node
              let aPos = toPosition pos
              fiAnchors %= (Anchor{..} :)
              loop (subs ++ nodes) toIgnore
            HTML_INLINE htmlText -> do
              let mName = T.stripSuffix "\">" =<< T.stripPrefix "<a name=\"" htmlText
              whenJust mName $ \aName -> do
                  let aType = HandAnchor
                      aPos = toPosition pos
                  fiAnchors %= (Anchor{..} :)
              processHtmlNode node pos nodes toIgnore
            LINK url _ -> do
              let rName = nodeExtractText node
                  rPos = toPosition pos
                  link = if null url then rName else url
              let (rLink, rAnchor) = case T.splitOn "#" link of
                      [t]    -> (t, Nothing)
                      t : ts -> (t, Just $ T.intercalate "#" ts)
                      []     -> error "impossible"
              fiReferences %= (Reference{..} :)
              loop nodes toIgnore
            _ -> loop (subs ++ nodes) toIgnore

    defNode :: Node
    defNode = Node Nothing DOCUMENT [] -- hard-coded default Node

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

          textToMode :: [Text] -> Maybe IgnoreMode
          textToMode ("ignore" : [x])
            | x == "link"      = return Link
            | x == "paragraph" = return Paragraph
            | x == "file"      = return File
            | otherwise        = Nothing
          textToMode _           = Nothing
      in textToMode . words =<< mContent

    isComment :: Node -> Bool
    isComment = isJust . getCommentContent

    isIgnoreFile :: Node -> Bool
    isIgnoreFile = (Just File ==) . getIgnoreMode

    checkIgnoreFile :: [Node] -> Bool
    checkIgnoreFile nodes =
      let isSimpleComment :: Node -> Bool
          isSimpleComment node = isComment node && not (isIgnoreFile node)

          mIgnoreFile = safeHead $ dropWhile isSimpleComment nodes
      in maybe False isIgnoreFile mIgnoreFile

    isLink :: Node -> Bool
    isLink (Node _ (LINK _ _) _) = True
    isLink _ = False

    isText :: Node -> Bool
    isText (Node _ (TEXT _) _) = True
    isText _ = False

    afterIgnoredLink :: [Node] -> Maybe [Node]
    afterIgnoredLink (fNode : nodes)
      | isLink fNode = return nodes
      | sNode : nodes' <- nodes =
          if isText fNode && isLink sNode
          then return nodes'
          else Nothing
      | otherwise = Nothing
    afterIgnoredLink _ = Nothing

    prettyPos :: Maybe PosInfo -> Text
    prettyPos pos =
      let posToText :: Position -> Text
          posToText (Position mPos) = fromMaybe "" mPos
      in "(" <> posToText (toPosition pos) <> ")"

    prettyType :: NodeType -> Text
    prettyType ty =
      let mType = safeHead $ words $ show ty
      in maybe "" id mType

    fileMsg :: Text
    fileMsg =
      "\"ignore file\" must be at the top of \
      \markdown or right after comments at the top"

    linkMsg :: Text
    linkMsg = "expected a LINK after \"ignore link\" "

    paragraphMsg :: Text -> Text
    paragraphMsg txt = unwords
      [ "expected a PARAGRAPH after \
         \\"ignore paragraph\", but found"
      , txt
      , ""
      ]

    unrecognisedMsg :: Text -> Text
    unrecognisedMsg txt = unwords
      [ "unrecognised option"
      , "\"" <> txt <> "\""
      , "perhaps you meant \
         \<\"ignore link\"|\"ignore paragraph\"|\"ignore file\"> "
      ]

    returnError
      :: Maybe IgnoreMode
      -> Text
      -> Maybe PosInfo
      -> StateT FileInfo (Except Text) ()
    returnError mode txt pos =
      let errMsg = case mode of
            Just Link      -> linkMsg
            Just Paragraph -> paragraphMsg txt
            Just File      -> fileMsg
            Nothing        -> unrecognisedMsg txt
          posInfo = prettyPos pos
      in lift $ throwE $ errMsg <> posInfo

    processHtmlNode
      :: Node
      -> Maybe PosInfo
      -> [Node]
      -> Maybe IgnoreMode
      -> StateT FileInfo (Except Text) ()
    processHtmlNode node pos nodes toIgnore = do
      let xrefcheckContent = getXrefcheckContent node
      case xrefcheckContent of
        Just content -> maybe (returnError Nothing content pos)
          (loop nodes . pure) $ getIgnoreMode node
        Nothing -> loop nodes toIgnore

parseFileInfo :: MarkdownConfig -> LT.Text -> Either Text FileInfo
parseFileInfo config input = runExcept $ nodeExtractInfo config $
  commonmarkToNode [] [] $ toStrict input

markdownScanner :: MarkdownConfig -> ScanAction
markdownScanner config path = do
  errOrInfo <- parseFileInfo config . decodeUtf8 <$> BSL.readFile path
  case errOrInfo of
    Left errTxt -> do
      die $ "Error when scanning " <> path <> ": " <> T.unpack errTxt
    Right fileInfo -> return fileInfo

markdownSupport :: MarkdownConfig -> ([Extension], ScanAction)
markdownSupport config = ([".md"], markdownScanner config)
