{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.CopyPasteCheckSpec where

import Universum

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import Xrefcheck.Core
import Xrefcheck.Verify

assertUnordered :: (Show a, Ord a) => [a] -> [a] -> Assertion
assertUnordered = (@?=) `on` sort

testPath :: FilePath
testPath = "test-path"

test_copyPasteCheck :: TestTree
test_copyPasteCheck = testGroup "Copypaste check"
  [ testCase "Detect copypaste error if there is a link with a matching name" $ do
      let link = "./first-file"
          anchor = Just "heading"
          differentAnchor = Nothing
          defPos = Position Nothing
          original1 = Reference "_-  First -  - File" link anchor defPos
          original2 = Reference "_-  First - fi - le" link anchor defPos
          notCopied = Reference " Link 2 " link differentAnchor defPos
          copied1 = Reference " foo bar" link anchor defPos
          copied2 = Reference " Baz quux" link anchor defPos
          input = [original1, original2, notCopied, copied1, copied2]
          res = checkCopyPaste testPath input
          expectedRes =
          -- only first matching link is shown in the output
            [ CopyPasteCheckResult testPath original1 copied1
            , CopyPasteCheckResult testPath original1 copied2
            ]
      res `assertUnordered` expectedRes
  , testCase "Succeed if there is not link with a matching name" $ do
      let link = "./first-file"
          anchor = Just "heading"
          defPos = Position Nothing
          original1 = Reference "_Foo bar" link anchor defPos
          original2 = Reference " Baz quux" link anchor defPos
          original3 = Reference " Foo qubarx" link anchor defPos
          input = [original1, original2, original3]
          res = checkCopyPaste testPath input
          expectedRes = []
      res @?= expectedRes
  , testCase "Check external links" $ do
      let link = "https://github.com"
          anchor = Nothing
          defPos = Position Nothing
          original = Reference "github" link anchor defPos
          copied = Reference "gitlab" link anchor defPos
          input = [original, copied]
          res = checkCopyPaste testPath input
          expectedRes =
            [ CopyPasteCheckResult testPath original copied
            ]
      res @?= expectedRes
  ]
