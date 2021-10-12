{- SPDX-FileCopyrightText: 2018-2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

-- | Generalised repo scanner and analyser.

module Xrefcheck.Scan
  ( TraversalConfig (..)
  , Extension
  , ScanAction
  , FormatsSupport
  , RepoInfo (..)
  , ScanResult (..)

  , gatherRepoInfo
  , specificFormatsSupport
  ) where

import Control.Exception.Safe (throwString)
import Data.Aeson.TH (deriveFromJSON)
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.Err (errorWithoutStackTrace)
import qualified System.Directory.Tree as Tree
import System.FilePath (takeDirectory, takeExtension, (</>))

import Control.Monad.Except (Except)
import Control.Monad.Trans.Except
import Xrefcheck.Core
import Xrefcheck.Progress
import Xrefcheck.Util (aesonConfigOption)

-- | Config of repositry traversal.
data TraversalConfig = TraversalConfig
  { tcIgnored   :: [FilePath]
    -- ^ Files and folders, files in which we completely ignore.
  }

deriveFromJSON aesonConfigOption ''TraversalConfig

-- | File extension, dot included.
type Extension = String

-- | Way to parse a file.
type ScanAction = FilePath -> ExceptT ScanError IO FileInfo

-- | All supported ways to parse a file.
type FormatsSupport = Extension -> Maybe ScanAction

type ScanError = (FilePath, Text)
data ScanResult e = ScanResult [e] RepoInfo
  deriving Show

specificFormatsSupport :: [([Extension], ScanAction)] -> FormatsSupport
specificFormatsSupport formats = \ext -> M.lookup ext formatsMap
  where
    formatsMap = M.fromList
        [ (extension, parser)
        | (extensions, parser) <- formats
        , extension <- extensions
        ]

gatherRepoInfo
  :: Rewrite
  -> FormatsSupport
  -> TraversalConfig
  -> FilePath
  -> IO (ScanResult ScanError)
gatherRepoInfo rw formatsSupport config root = do
  putTextRewrite rw "Scanning repository..."
  _ Tree.:/ repoTree <- Tree.readDirectoryWithL processFile rootNE
  let (fileErrs, excluded) = filterExcludedDirs root repoTree
  let fileInfos = filter (\(path, _) -> not $ isIgnored path)
        $ dropSndMaybes . F.toList
        $ Tree.zipPaths . (dirOfRoot Tree.:/)
        $ excluded
  return . ScanResult fileErrs $ RepoInfo (M.fromList fileInfos)
  where
    rootNE = if null root then "." else root
    dirOfRoot = if root == "" || root == "." then "" else takeDirectory root

    processFile :: FilePath -> IO (Except ScanError FileInfo)
    processFile file = do
      let ext = takeExtension file
      case runExceptT <$> (formatsSupport ext <*> Just file) of
        Just mscanner -> except <$> mscanner
        Nothing -> return $ throwE (file, "Unknown error")

    dropSndMaybes l = [(a, b) | (a, Just b) <- l]

    ignored = map (root </>) (tcIgnored config)
    isIgnored path = path `elem` ignored
    filterExcludedDirs cur = \case
      Tree.Dir name subfiles ->
        let subfiles'
              | isIgnored cur = []
              | otherwise = map visitRec subfiles
            visitRec sub = filterExcludedDirs (cur </> Tree.name sub) sub
        in Tree.Dir name subfiles'
      file@Tree.File{} -> file
      Tree.Failed _name err ->
        errorWithoutStackTrace $ "Repository traversal failed: " <> show err
