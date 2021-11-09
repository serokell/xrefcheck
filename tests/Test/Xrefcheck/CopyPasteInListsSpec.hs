{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.CopyPasteInListsSpec where

import Universum

import Test.Hspec (Spec, describe, it, shouldBe)

import Test.Xrefcheck.Util
import Xrefcheck.Core

spec :: Spec
spec = do
  describe "Possibly incorrect copy-paste" $ do
    for_ allFlavors $ \fl -> do
      it ("is detected (" <> show fl <> ")") $ do
        fi <- getFI fl "tests/markdowns/without-annotations/copy-paste_in_lists.md"
        getPasta fi `shouldBe`[("a", "c")]
  where
    getPasta :: FileInfo -> [(Text, Text)]
    getPasta fi = map (cpAnchorText &&& cpPlainText) $ fi ^. fiCopyPastes
