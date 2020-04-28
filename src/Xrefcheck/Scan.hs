{- SPDX-FileCopyrightText: 2018-2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

-- | Generalised repo scanner and analyser.

module Xrefcheck.Scan
    ( Extension
    , ScanAction
    , FormatsSupport
    , RepoInfo (..)

    , gatherRepoInfo
    , specificFormatsSupport
    ) where

import qualified Data.Foldable as F
import qualified Data.Map as M
import GHC.Err (errorWithoutStackTrace)
import qualified System.Directory.Tree as Tree
import System.FilePath (takeDirectory, takeExtension, (</>))

import Xrefcheck.Config
import Xrefcheck.Core
import Xrefcheck.Progress
import Xrefcheck.Util ()

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
    => Rewrite -> FormatsSupport -> TraversalConfig -> FilePath -> m RepoInfo
gatherRepoInfo rw formatsSupport config root = do
    putTextRewrite rw "Scanning repository..."
    _ Tree.:/ repoTree <- liftIO $ Tree.readDirectoryWithL processFile rootNE
    let fileInfos = filter (\(path, _) -> not $ isIgnored path) $
                    dropSndMaybes . F.toList $
                    Tree.zipPaths . (dirOfRoot Tree.:/) $
                    filterExcludedDirs root repoTree
    return $ RepoInfo (M.fromList fileInfos)
  where
    rootNE = if null root then "." else root
    dirOfRoot = if root == "" || root == "." then "" else takeDirectory root
    processFile file = do
        let ext = takeExtension file
        let mscanner = formatsSupport ext
        forM mscanner $ \scanFile ->
            scanFile file
    dropSndMaybes l = [(a, b) | (a, Just b) <- l]

    ignored = map (root </>) (tcIgnored config)
    isIgnored path = path `elem` ignored
    filterExcludedDirs cur = \case
        Tree.Dir name subfiles ->
            let subfiles' =
                  if isIgnored cur
                  then []
                  else map visitRec subfiles
                visitRec sub = filterExcludedDirs (cur </> Tree.name sub) sub
            in Tree.Dir name subfiles'
        file@Tree.File{} -> file
        Tree.Failed _name err ->
            errorWithoutStackTrace $ "Repository traversal failed: " <> show err
