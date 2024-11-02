{- SPDX-FileCopyrightText: 2023 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

-- | Scanner for gathering references to verify from symlinks.
--
-- A symlink's reference corresponds to the file it points to.
module Xrefcheck.Scanners.Symlink
  ( symlinkScanner
  , symlinkSupport
  ) where

import Universum

import Data.Reflection (Given)
import System.Directory (getSymbolicLinkTarget)

import Xrefcheck.Core
import Xrefcheck.Scan
import Xrefcheck.System

symlinkScanner :: Given PrintUnixPaths => ScanAction
symlinkScanner root relativePath = do
  let rootedPath = filePathFromRoot root relativePath
      pathForPrinting = mkPathForPrinting rootedPath
  rLink <- unRelPosixLink . mkRelPosixLink
    <$> getSymbolicLinkTarget rootedPath

  let rName = "Symbolic Link"
      rPos = Position (fromString pathForPrinting)
      rInfo = referenceInfo rLink

  pure (FileInfo [Reference {rName, rPos, rInfo}] [], [])

symlinkSupport :: Given PrintUnixPaths => FileSupport
symlinkSupport isSymlink _ = do
  guard isSymlink
  pure symlinkScanner
