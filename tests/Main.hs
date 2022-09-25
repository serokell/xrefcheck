-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Main
  ( main
  ) where

import Universum

import Test.Tasty
import Tree (tests)

main :: IO ()
main = tests >>= defaultMain
