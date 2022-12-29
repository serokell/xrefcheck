{- SPDX-FileCopyrightText: 2018-2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Main where

import Universum

import Main.Utf8 (withUtf8)
import System.IO.CodePage (withCP65001)

import System.Directory (doesFileExist)
import Xrefcheck.CLI (Command (..), DumpConfigMode (..), getCommand)
import Xrefcheck.Command (defaultAction)
import Xrefcheck.Config (defConfigText)

main :: IO ()
main = withUtf8 $ withCP65001 $ do
  command <- getCommand
  case command of
    DefaultCommand options ->
      defaultAction options
    DumpConfig repoType (DCMFile forceFlag path) -> do
      whenM ((not forceFlag &&) <$> doesFileExist path) do
        putTextLn "Output file exists. Use --force to overwrite."
        exitFailure
      writeFile path (defConfigText repoType)
    DumpConfig repoType DCMStdout ->
      putStr (defConfigText repoType)
