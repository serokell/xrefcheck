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

import System.Directory (getSymbolicLinkTarget)

import Xrefcheck.Core
import Xrefcheck.Scan
import Xrefcheck.System

symlinkScanner :: ScanAction
symlinkScanner root path = do
  rLink <- unRelPosixLink . mkRelPosixLink
    <$> getSymbolicLinkTarget (filePathFromRoot root path)

  let rName = "Symbolic Link"
      rPos = Position Nothing
      rInfo = referenceInfo rLink

  pure (FileInfo [Reference {rName, rPos, rInfo}] [], [])

symlinkSupport :: FileSupport
symlinkSupport isSymlink _ = do
  guard isSymlink
  pure symlinkScanner
