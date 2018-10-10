-- | Markdown documents markdownScanner.

module Crv.Scanners.Markdown
    ( markdownScanner
    , markdownSupport
    ) where

import Control.Lens ((%=))
import Data.Default (Default (..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import GHC.Conc (par)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

import Crv.Core
import Crv.Scan

type MonadFileScan m = (P.MonadParsec () LT.Text m, MonadState FileInfo m)

inBrackets :: MonadFileScan m => LT.Text -> LT.Text -> m Text
inBrackets open close = do
    _ <- P.string open
    res <- P.takeWhileP (Just "in brackets") (/= LT.head close)
    _ <- P.string close
    return $ toStrict res

fileInfoParser :: (MonadFileScan m) => m ()
fileInfoParser = loop >> modify finaliseFileInfo
  where
    loop = do
        asum
            [ P.try parseReference
            , P.try parseBiblioRef
            , P.try parseFootnoteRef
            , P.try parseHandAnchor
            , P.try parseSectionName
            , P.try parseBiblioAnchor
            , skipQuotes
            , skipCodeBlocks
            , skipUninteresting
            ]
        unlessM P.atEnd loop

    parseReference = do
        rName <- inBrackets "[" "]"
        rawLink <- inBrackets "(" ")"

        let link = if null rawLink then rName else rawLink
        let (rLink, rAnchor) = case T.splitOn "#" link of
                [t]    -> (t, Nothing)
                t : ts -> (t, Just $ T.intercalate "#" ts)
                []     -> error "impossible"

        fiReferences %= (Reference{..} :)

    parseBiblioRef = do
        rName <- inBrackets "[" "]"
        anchor <- inBrackets "[" "]"
        let rAnchor = Just $ if null anchor then rName else anchor
        let rLink = ""
        fiReferences %= (Reference{..} :)

    parseFootnoteRef = do
        rName <- inBrackets "[^" "]"
        let rAnchor = Just ("^" <> rName)
            rLink = ""
        fiReferences %= (Reference{..} :)

    parseHandAnchor = do
        aName <- inBrackets "<a name=\"" "\"></a>"
        let aType = HandAnchor
        fiAnchors %= (Anchor{..} :)

    parseBiblioAnchor = do
        aName <- inBrackets "[" "]:"
        let aType = BiblioAnchor
        fiAnchors %= (Anchor{..} :)

    parseSectionName = do
        prefix <- P.takeWhile1P (Just "header prefix") (== '#')
        rawName <- P.takeWhileP (Just "header") $ \c -> all (/= c) ['\r', '\n']
        _ <- P.eol
        let aType = HeaderAnchor (length prefix)
            aName = headerToAnchor $
                    T.dropWhileEnd (\c -> c == '#' || c == ' ') $ T.strip $
                    toStrict rawName
        fiAnchors %= (Anchor{..} :)

    skipQuotes = do
        _ <- P.char '`'
        P.skipMany (void (P.string "\\\\`") <|> void (P.anySingleBut '`'))
        _ <- P.char '`'
        return ()

    skipCodeBlocks = void $ inBrackets "```" "```"

    skipUninteresting = do
        _ <- P.anySingle
        P.skipMany $ P.satisfy (\c -> all (/= c) ['[', '<', '#', '`'])

parseFileInfo :: FilePath -> LT.Text -> FileInfo
parseFileInfo path input =
    let outcome = P.parse (execStateT fileInfoParser def) path input
    in case outcome of
        Left err  -> error $ "Failed to parse file " <> show path <>
                             ": " <> show err
        Right res -> res

markdownScanner :: ScanAction
markdownScanner path = liftIO $ do
    res <- parseFileInfo path <$> LT.readFile path
    force res `par` return res

markdownSupport :: ([Extension], ScanAction)
markdownSupport = ([".md"], markdownScanner)
