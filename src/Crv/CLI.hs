{-# LANGUAGE ApplicativeDo #-}

module Crv.CLI
    ( Options (..)
    , getOptions
    ) where

import Data.Version (showVersion)
import Options.Applicative (Parser, execParser, fullDesc, help, helper, info, infoOption, long,
                            metavar, progDesc, short, strOption, value)
import Paths_crossref_verifier (version)

data Options = Options
    { oConfig :: FilePath
    , oRoot   :: FilePath
    }

optionsParser :: Parser Options
optionsParser = do
    oConfig <- strOption $
        short 'c' <>
        long "config" <>
        metavar "FILEPATH" <>
        help "Path to configuration file." <>
        value ".crossref-verifier.yaml"
    oRoot <- strOption $
        short 'r' <>
        long "root" <>
        metavar "DIRECTORY" <>
        help "Path to repository root." <>
        value ""
    return Options{..}

versionOption :: Parser (a -> a)
versionOption = infoOption ("crossref-verify-" <> (showVersion version)) $
    long "version" <>
    help "Show version."

getOptions :: IO Options
getOptions = do
    execParser $
        info (helper <*> versionOption <*> optionsParser) $
        fullDesc <>
        progDesc "Github repository cross-references verifier."
