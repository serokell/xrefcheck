-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Main
  ( main
  ) where

import Universum

import Test.Tasty
import Test.Tasty.Ingredients (Ingredient)
import Test.Xrefcheck.Util (mockServerOptions)
import Tree (tests)

main :: IO ()
main = tests >>= defaultMainWithIngredients ingredients

ingredients :: [Ingredient]
ingredients = includingOptions mockServerOptions : defaultIngredients
