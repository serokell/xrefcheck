{- SPDX-FileCopyrightText: 2018-2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE ApplicativeDo #-}

module Xrefcheck.CLI
  ( VerifyMode (..)
  , shouldCheckLocal
  , shouldCheckExternal
  , Command (..)
  , Options (..)
  , VerifyOptions (..)
  , addVerifyOptions
  , TraversalOptions (..)
  , addTraversalOptions
  , defaultConfigPaths
  , getCommand
  ) where

import Universum

import Data.Char qualified as C
import Data.List qualified as L
import Data.Text qualified as T
import Data.Version (showVersion)
import Options.Applicative
  (Parser, ReadM, command, eitherReader, execParser, flag, flag', footerDoc, fullDesc, help, helper,
  hsubparser, info, infoOption, long, metavar, option, progDesc, short, strOption, switch, value, auto,
  OptionFields, Mod)
import Options.Applicative.Help.Pretty (Doc, displayS, fill, fillSep, indent, renderPretty, text)

import Paths_xrefcheck (version)
import Xrefcheck.Config (VerifyConfig (..))
import Xrefcheck.Core
import Xrefcheck.Scan
import Xrefcheck.Util (normaliseWithNoTrailing)

modeReadM :: ReadM VerifyMode
modeReadM = eitherReader $ \s ->
  case find ((== s) . fst) modes of
    Just (_, mode) -> Right mode
    Nothing -> Left . mconcat $ intersperse "\n"
      [ "Unknown mode " <> show s <> "."
      , "Allowed values: " <> mconcat (intersperse ", " $ map (show . fst) modes)
      ]
  where
    modes =
      [ ("local-only", LocalOnlyMode)
      , ("external-only", ExternalOnlyMode)
      , ("full", FullMode)
      ]

data Command
  = DefaultCommand Options
  | DumpConfig Flavor FilePath

data Options = Options
  { oConfigPath       :: Maybe FilePath
  , oRoot             :: FilePath
  , oMode             :: VerifyMode
  , oVerbose          :: Bool
  , oShowProgressBar  :: Maybe Bool
  , oTraversalOptions :: TraversalOptions
  , oVerifyOptions    :: VerifyOptions
  }

data TraversalOptions = TraversalOptions
  { toIgnored :: [FilePath]
  }

addTraversalOptions :: TraversalConfig -> TraversalOptions -> TraversalConfig
addTraversalOptions TraversalConfig{..} (TraversalOptions ignored) =
  TraversalConfig
  { tcIgnored = tcIgnored ++ ignored
  , ..
  }

data VerifyOptions = VerifyOptions
  { voCheckLocalhost :: Maybe Bool
  , voMaxRetries     :: Maybe Int
  }

addVerifyOptions :: VerifyConfig -> VerifyOptions -> VerifyConfig
addVerifyOptions VerifyConfig{..} (VerifyOptions checkLocalhost maxRetries) =
  VerifyConfig
  { vcCheckLocalhost = fromMaybe vcCheckLocalhost checkLocalhost
  , vcMaxRetries = fromMaybe vcMaxRetries maxRetries
  , ..
  }

-- | Where to try to seek configuration if specific path is not set.
defaultConfigPaths :: [FilePath]
defaultConfigPaths = ["./xrefcheck.yaml", "./.xrefcheck.yaml"]

-- | Strictly speaking, what config we will dump depends on the repository type:
-- this affects Markdown flavor, things excluded by default, e.t.c.
--
-- But at the moment there is one-to-one correspondence between repository types
-- and flavors, so we write a type alias here.
type RepoType = Flavor

filepathOption :: Mod OptionFields FilePath -> Parser FilePath
filepathOption = fmap normaliseWithNoTrailing <$> strOption

repoTypeReadM :: ReadM RepoType
repoTypeReadM = eitherReader $ \name ->
  maybeToRight (failureText name) $ L.lookup (map C.toLower name) allRepoTypesNamed
  where
    allRepoTypesNamed =
      allRepoTypes <&> \ty -> (toString $ T.toLower (show ty), ty)
    failureText name =
      "Unknown repository type: " <> show name <> "\n\
      \Expected one of: " <> mconcat (intersperse ", " $ map show allRepoTypes)
    allRepoTypes = allFlavors

optionsParser :: Parser Options
optionsParser = do
  oConfigPath <- optional . filepathOption $
    short 'c' <>
    long "config" <>
    metavar "FILEPATH" <>
    help ("Path to configuration file. \
          \If not specified, tries to read config from one of " <>
          (mconcat . intersperse ", " $ map show defaultConfigPaths) <> ". \
          \If none of these files exist, default configuration is used."
         )
  oRoot <- filepathOption $
    short 'r' <>
    long "root" <>
    metavar "DIRECTORY" <>
    help "Path to repository root." <>
    value "."
  oMode <- option modeReadM $
    short 'm' <>
    long "mode" <>
    metavar "KEYWORD" <>
    value FullMode <>
    help "Which parts of verification to invoke. \
         \You can enable only verification of repository-local references, \
         \only verification of external references or both. \
         \Default mode: full."
  oVerbose <- switch $
    short 'v' <>
    long "verbose" <>
    help "Report repository scan and verification details."
  oShowProgressBar <- asum
    [ flag' (Just True) $
        long "progress" <>
        help "Display progress bar during verification. \
             \This is enabled by default unless `CI` env var is set to true."
    , flag' (Just False) $
        long "no-progress" <>
        help "Do not display progress bar during verification."
    , pure Nothing
    ]
  oTraversalOptions <- traversalOptionsParser
  oVerifyOptions <- verifyOptionsParser
  return Options{..}

traversalOptionsParser :: Parser TraversalOptions
traversalOptionsParser = do
  toIgnored <- many . filepathOption $
    long "ignored" <>
    metavar "FILEPATH" <>
    help "Files and folders which we pretend do not exist."
  return TraversalOptions{..}

verifyOptionsParser :: Parser VerifyOptions
verifyOptionsParser = do
  voCheckLocalhost <- flag Nothing (Just True) $
    long "check-localhost" <>
    help "Check localhost links."
  voMaxRetries <- option (Just <$> auto) $
    long "retries" <>
    metavar "INT" <>
    value Nothing <>
    help "How many attempts to retry an external link after getting \
         \a \"429 Too Many Requests\" response."
  return VerifyOptions{..}

dumpConfigOptions :: Parser Command
dumpConfigOptions = hsubparser $
  command "dump-config" $
    info parser $
    progDesc "Dump default configuration into a file."
  where
    parser = DumpConfig <$> repoTypeOption <*> outputOption

    repoTypeOption =
      option repoTypeReadM $
      short 't' <>
      long "type" <>
      metavar "REPOSITORY TYPE" <>
      help "Git repository type."

    outputOption =
      filepathOption $
      short 'o' <>
      long "output" <>
      metavar "FILEPATH" <>
      value ".xrefcheck.yaml" <>
      help "Name of created config file."

totalParser :: Parser Command
totalParser = asum
  [ DefaultCommand <$> optionsParser
  , dumpConfigOptions
  ]

versionOption :: Parser (a -> a)
versionOption = infoOption ("xrefcheck-" <> showVersion version) $
  long "version" <>
  help "Show version."

getCommand :: IO Command
getCommand = do
  execParser $
    info (helper <*> versionOption <*> totalParser) $
    fullDesc <>
    progDesc "Cross-references verifier for markdown documentation in \
             \Git repositories." <>
    footerDoc (pure ignoreModesMsg)

ignoreModesMsg :: Doc
ignoreModesMsg = text $ header <> body
  where
    header = "To ignore a link in your markdown, \
             \include \"<!-- xrefcheck: ignore <mode> -->\"\n\
             \comment with one of these modes:\n"
    body = displayS (renderPretty pageParam pageWidth doc) ""

    pageWidth = 80
    pageParam = 1

    doc = fillSep $ map formatDesc modeDescr

    modeDescr =
      [ ("  \"link\"",      L.words "Ignore the link right after the comment.")
      , ("  \"paragraph\"", L.words "Ignore the whole paragraph after the comment.")
      , ("  \"file\"",      L.words "This mode can only be used at the top of \
                                    \markdown or right after comments at the top.")
      ]

    modeIndent = length ("\"paragraph\"" :: String) + 2
    descrIndent = 27 - modeIndent

    formatDesc (mode, descr) =
      fill modeIndent (text mode) <>
      indent descrIndent (fillSep $ map text descr)
