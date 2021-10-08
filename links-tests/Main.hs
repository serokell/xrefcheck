-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Main
  ( main
  ) where

import Test.Tasty (defaultIngredients, defaultMainWithIngredients, includingOptions)
import Test.Tasty.Ingredients (Ingredient)

import Test.Xrefcheck.FtpLinks (ftpOptions)
import Tree (tests)

main :: IO ()
main = tests >>= defaultMainWithIngredients ingredients

ingredients :: [Ingredient]
ingredients = includingOptions ftpOptions : defaultIngredients
