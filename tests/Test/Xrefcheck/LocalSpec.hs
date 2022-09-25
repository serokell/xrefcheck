{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.LocalSpec where

import Universum

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Xrefcheck.Core (canonizeLocalRef)

test_local_refs_canonizing :: TestTree
test_local_refs_canonizing = testGroup "Local refs canonizing" $
    [ testCase "Strips ./" $
        canonizeLocalRef "./AnchorsSpec.hs" @?= "AnchorsSpec.hs"

    , testCase "Strips ././" $
        canonizeLocalRef "././AnchorsSpec.hs" @?= "AnchorsSpec.hs"

    , testCase "Leaves plain other intact" $
        canonizeLocalRef "../AnchorsSpec.hs" @?= "../AnchorsSpec.hs"
    ]
