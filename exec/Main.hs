{- SPDX-FileCopyrightText: 2018-2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Main where

import Universum

import Data.ByteString qualified as BS
import Main.Utf8 (withUtf8)

import Xrefcheck.CLI (Command (..), getCommand)
import Xrefcheck.Command (defaultAction)
import Xrefcheck.Config (defConfigText)

main :: IO ()
main = withUtf8 $ do
  command <- getCommand
  case command of
    DefaultCommand options ->
      defaultAction options
    DumpConfig repoType path ->
      BS.writeFile path (defConfigText repoType)
