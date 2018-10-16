{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Markdown documents markdownScanner.

module Crv.Scanners.Markdown
    ( markdownScanner
    , markdownSupport
    ) where

import CMarkGFM (Node (..), NodeType (..), commonmarkToNode)
import Control.Lens ((%=))
import qualified Data.ByteString.Lazy as BSL
import Data.Default (Default (..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Fmt (Buildable (..), blockListF, nameF)
import GHC.Conc (par)

import Crv.Core
import Crv.Scan

instance Buildable Node where
    build (Node _mpos ty subs) = nameF (show ty) $ blockListF subs

nodeFlatten :: Node -> [NodeType]
nodeFlatten (Node _pos ty subs) = ty : concatMap nodeFlatten subs

nodeExtractText :: Node -> Text
nodeExtractText = mconcat . map extractText . nodeFlatten
  where
    extractText = \case
        TEXT t -> t
        CODE t -> t
        _ -> ""

nodeExtractInfo :: Node -> ExceptT Text Identity FileInfo
nodeExtractInfo docNode = fmap finaliseFileInfo $ execStateT (loop docNode) def
  where
    loop node@(Node _pos ty subs) = case ty of
        DOCUMENT ->
            mapM_ loop subs
        PARAGRAPH ->
            mapM_ loop subs
        HEADING lvl ->
            let text = nodeExtractText node
                aType = HeaderAnchor lvl
                aName = headerToAnchor text
            in fiAnchors %= (Anchor{..} :)
        LIST _ ->
            mapM_ loop subs
        ITEM ->
            mapM_ loop subs
        HTML_INLINE htmlText -> do
            let mName = T.stripSuffix "\">" =<< T.stripPrefix "<a name=\"" htmlText
            whenJust mName $ \aName -> do
                let aType = HandAnchor
                fiAnchors %= (Anchor{..} :)
        LINK url _ -> do
            let rName = nodeExtractText node
                link = if null url then rName else url
            let (rLink, rAnchor) = case T.splitOn "#" link of
                    [t]    -> (t, Nothing)
                    t : ts -> (t, Just $ T.intercalate "#" ts)
                    []     -> error "impossible"
            fiReferences %= (Reference{..} :)
        _ -> pass

parseFileInfo :: FilePath -> LT.Text -> FileInfo
parseFileInfo path input =
    let outcome = runIdentity . runExceptT $
                  nodeExtractInfo $ commonmarkToNode [] [] $ toStrict input
    in case outcome of
        Left err  -> error $ "Failed to parse file " <> show path <>
                             ": " <> show err
        Right res -> res

markdownScanner :: ScanAction
markdownScanner path = liftIO $ do
    res <- parseFileInfo path . decodeUtf8 <$> BSL.readFile path
    force res `par` return res

markdownSupport :: ([Extension], ScanAction)
markdownSupport = ([".md"], markdownScanner)
