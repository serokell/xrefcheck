{- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.CanonicalPathSpec where

import Universum

import System.Directory (getCurrentDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Xrefcheck.System

test_canonicalPath :: IO TestTree
test_canonicalPath = do
  current <- getCurrentDirectory >>= canonicalizePath
  return $ testGroup "Canonical paths"
    [ testGroup "Canonicalization"
        [ testCase "Trailing separator" $ do
            path <- canonicalizePath "./example/dir/"
            getPosixRelativeOrAbsoluteChild current path @?= "example/dir"
        , testCase "Parent directory indirection" $ do
            path <- canonicalizePath "dir1/../dir2"
            getPosixRelativeOrAbsoluteChild current path @?= "dir2"
        , testCase "Through parent directory indirection" $ do
            path <- canonicalizePath "dir1/../../../dir2"
            root <- current </ "../.."
            getPosixRelativeOrAbsoluteChild root path @?= "dir2"
        , testCase "Current directory indirection" $ do
            path <- canonicalizePath "././dir1/./././dir2/././"
            getPosixRelativeOrAbsoluteChild current path @?= "dir1/dir2"
        , testCase "Mixed indirections result in current directory" $ do
            path <- canonicalizePath "././dir1/./.././dir2/./../"
            getPosixRelativeOrAbsoluteChild current path @?= "."
        ]
    , testGroup "Relative path"
        [ testCase "Child directory" $ do
            path <- canonicalizePath "./dir1/dir2/"
            getPosixRelativeChild current path @?= Just ("dir1/dir2")
        , testCase "Not a child directory" $ do
            root <- canonicalizePath "./dir1/dir2/"
            path <- canonicalizePath "./dir1/dir3/"
            getPosixRelativeChild root path @?= Nothing
        ]
    , testGroup "Intermediate directories"
      [ testCase "No intermediate directories same" $ do
          path <- canonicalizePath "./dir1/dir3/"
          getDirsBetweenRootAndFile path path @?= []
      , testCase "No intermediate directories different" $ do
          root <- canonicalizePath "./dir1/dir2/"
          path <- canonicalizePath "./dir1/dir3/"
          getDirsBetweenRootAndFile root path @?= []
      , testCase "Intermediate directories" $ do
          path <- canonicalizePath "./example/dir/other"
          getPosixRelativeOrAbsoluteChild current <$> getDirsBetweenRootAndFile current path @?=
            [ "."
            , "example"
            , "example/dir"
            ]
      ]
    ]
