{- SPDX-FileCopyrightText: 2023 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.CanonicalRelPosixLinkSpec where

import Universum

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))

import Xrefcheck.System

test_canonicalRelPosixLink :: TestTree
test_canonicalRelPosixLink =
  testGroup "Canonical relative POSIX links"
    [ testGroup "Normalization"
        [ testCase "Trailing separator" $
            on (@?=) mkCanonicalLink "./example/dir/" "example/dir"
        , testCase "Parent directory indirection" $
            on (@?=) mkCanonicalLink "dir1/../dir2" "dir2"
        , testCase "Through parent directory indirection" $
            hasUnexpanededParentIndirections (mkCanonicalLink "dir1/../../../dir2") @? "Unexpanded indirections"
        , testCase "Current directory indirection" $
            on (@?=) mkCanonicalLink "././dir1/./././dir2/././" "dir1/dir2"
        , testCase "Mixed indirections result in current directory" $
            on (@?=) mkCanonicalLink "././dir1/./.././dir2/./../" "."
        ]
    , testGroup "Intermediate directories"
      [ testCase "Current directory itself" $
          on (@?=) (fmap canonicalizeRelPosixLink) (getIntermediateDirs (mkRelPosixLink ".")) $
            fmap mkRelPosixLink ["."]
      , testCase "Current directory file" $
          on (@?=) (fmap canonicalizeRelPosixLink) (getIntermediateDirs (mkRelPosixLink "./file")) $
            fmap mkRelPosixLink ["."]
      , testCase "Parent directory itself" $
          on (@?=) (fmap canonicalizeRelPosixLink) (getIntermediateDirs (mkRelPosixLink "..")) $
            fmap mkRelPosixLink ["."]
      , testCase "Parent directory file" $
          on (@?=) (fmap canonicalizeRelPosixLink) (getIntermediateDirs (mkRelPosixLink "../file")) $
            fmap mkRelPosixLink [".", ".."]
      , testCase "Intermediate directories" $
          on (@?=) (fmap canonicalizeRelPosixLink) (getIntermediateDirs (mkRelPosixLink "./example/dir/file")) $
            fmap mkRelPosixLink [".", "example", "example/dir"]
      ]
    ]
  where
    mkCanonicalLink = canonicalizeRelPosixLink . mkRelPosixLink
