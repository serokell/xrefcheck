-- | Generalised repo scanner and analyser.

module Crv.Scan
    ( Extension
    , ScanAction
    , FormatsSupport
    , RepoInfo (..)

    , gatherRepoInfo
    , specificFormatsSupport
    ) where

import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified System.Directory.Tree as Tree
import System.FilePath.Posix (takeDirectory, takeExtension, (</>))

import Crv.Config
import Crv.Core
import Crv.Util ()

-- | File extension, dot included.
type Extension = String

-- | Way to parse a file.
type ScanAction = FilePath -> IO FileInfo

-- | All supported ways to parse a file.
type FormatsSupport = Extension -> Maybe ScanAction

specificFormatsSupport :: [([Extension], ScanAction)] -> FormatsSupport
specificFormatsSupport formats = \ext -> M.lookup ext formatsMap
  where
    formatsMap = M.fromList
        [ (extension, parser)
        | (extensions, parser) <- formats
        , extension <- extensions
        ]

gatherRepoInfo
    :: MonadIO m
    => FormatsSupport -> TraversalConfig -> FilePath -> m RepoInfo
gatherRepoInfo formatsSupport config root = do
    _ Tree.:/ repoTree <- liftIO $ Tree.readDirectoryWithL processFile rootNE
    let fileInfos = filter (\(path, _) -> not $ isExcluded path) $
                    dropSndMaybes . F.toList $
                    Tree.zipPaths . (dirOfRoot Tree.:/) $
                    filterExcludedDirs root repoTree
    return $ RepoInfo (M.fromList fileInfos)
  where
    rootNE = if null root then "." else root
    dirOfRoot = if null root then "" else takeDirectory root
    processFile file = do
        let ext = takeExtension file
        forM (formatsSupport ext) $ \scanFile ->
            scanFile file
    dropSndMaybes l = [(a, b) | (a, Just b) <- l]

    excluded = map (root </>) (tcExcluded config)
    isExcluded path = any (`isPrefixOf` path) excluded
    filterExcludedDirs cur = \case
        Tree.Dir name subfiles ->
            let subfiles' =
                  if isExcluded cur
                  then []
                  else map visitRec subfiles
                visitRec sub = filterExcludedDirs (cur </> Tree.name sub) sub
            in Tree.Dir name subfiles'
        other -> other
