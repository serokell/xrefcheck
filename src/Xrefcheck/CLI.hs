{- SPDX-FileCopyrightText: 2018-2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE ApplicativeDo #-}

module Xrefcheck.CLI
  ( VerifyMode (..)
  , ExclusionOptions (..)
  , Command (..)
  , DumpConfigMode (..)
  , Options (..)
  , NetworkingOptions (..)

  , addNetworkingOptions
  , shouldCheckLocal
  , shouldCheckExternal
  , addExclusionOptions
  , defaultConfigPaths
  , getCommand
  ) where

import Universum

import Data.Char qualified as C
import Data.List qualified as L
import Data.Text qualified as T
import Data.Version (showVersion)
import Options.Applicative
  (Mod, OptionFields, Parser, ReadM, auto, command, eitherReader, execParser, flag, flag',
  footerDoc, fullDesc, help, helpDoc, helper, hsubparser, info, infoOption, long, metavar, option,
  progDesc, short, strOption, switch, value)
import Options.Applicative.Help.Pretty (Doc, fill, fillSep, indent, pretty)
import Options.Applicative.Help.Pretty qualified as Pretty
import Text.Interpolation.Nyan

import Paths_xrefcheck (version)
import Xrefcheck.Config (NetworkingConfig, NetworkingConfig' (..))
import Xrefcheck.Core
import Xrefcheck.Scan
import Xrefcheck.System (CanonicalRelGlobPattern, mkCanonicalRelGlobPattern)
import Xrefcheck.Util (ColorMode (WithColors, WithoutColors))

modeReadM :: ReadM VerifyMode
modeReadM = eitherReader $ \s ->
  case find (\mi -> miName mi == s) modes of
    Just mi -> Right $ miMode mi
    Nothing -> Left
      [int||
      Unknown mode #s{s}.
      Allowed values: #{intercalate ", " $ map (show . miName) modes}.
      |]


data ModeInfo = ModeInfo
  { miName :: String
  , miMode :: VerifyMode
  , miHelpText :: String
  }

modes :: [ModeInfo]
modes =
  [ ModeInfo "local-only" LocalOnlyMode
      "Verify only references to local files."
  , ModeInfo "external-only" ExternalOnlyMode
      "Verify only external references (e.g. http or ftp URLs)."
  , ModeInfo "full" FullMode
      "Verify all references."
  ]

data Command
  = DefaultCommand Options
  | DumpConfig Flavor DumpConfigMode

data DumpConfigMode
  = DCMFile Bool FilePath
  | DCMStdout

data Options = Options
  { oConfigPath        :: Maybe FilePath
  , oRoot              :: FilePath
  , oMode              :: VerifyMode
  , oVerbose           :: Bool
  , oShowProgressBar   :: Maybe Bool
  , oColorMode         :: Maybe ColorMode
  , oExclusionOptions  :: ExclusionOptions
  , oNetworkingOptions :: NetworkingOptions
  , oScanPolicy        :: ScanPolicy
  }

data ExclusionOptions = ExclusionOptions
  { eoIgnore :: [CanonicalRelGlobPattern]
  }

addExclusionOptions :: ExclusionConfig -> ExclusionOptions -> ExclusionConfig
addExclusionOptions ExclusionConfig{..} (ExclusionOptions ignore) =
  ExclusionConfig
  { ecIgnore = ecIgnore ++ ignore
  , ..
  }

data NetworkingOptions = NetworkingOptions
  { noMaxRetries :: Maybe Int
  }

addNetworkingOptions :: NetworkingConfig -> NetworkingOptions -> NetworkingConfig
addNetworkingOptions NetworkingConfig{..} (NetworkingOptions maxRetries) =
  NetworkingConfig
  { ncMaxRetries = fromMaybe ncMaxRetries maxRetries
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
filepathOption = strOption

globOption :: Mod OptionFields CanonicalRelGlobPattern -> Parser CanonicalRelGlobPattern
globOption = option $ eitherReader mkCanonicalRelGlobPattern

repoTypeReadM :: ReadM RepoType
repoTypeReadM = eitherReader $ \name ->
  maybeToRight (failureText name) $ L.lookup (map C.toLower name) allRepoTypesNamed
  where
    allRepoTypesNamed =
      allRepoTypes <&> \ty -> (toString $ T.toLower (show ty), ty)
    failureText name =
      [int||
      Unknown repository type: #s{name}
      Expected one of: #{intercalate ", " $ map show allRepoTypes}.
      |]
    allRepoTypes = allFlavors

optionsParser :: Parser Options
optionsParser = do
  oConfigPath <- optional . filepathOption $
    short 'c' <>
    long "config" <>
    metavar "FILEPATH" <>
    help
      [int||
      Path to configuration file. \
      If not specified, tries to read config from one of \
      #{intercalate ", " $ map show defaultConfigPaths}. \
      If none of these files exist, default configuration is used.
      |]
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
    helpDoc
      ( Just $ Pretty.vsep $
          (modes <&> \mi -> fromString $ miName mi <> ": " <> miHelpText mi)
          <>
          [ "Default mode: full."]
      )
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
  oColorMode <- asum
    [ flag' (Just WithColors) $
        long "color" <>
        help "Enable ANSI coloring of output. \
            \When `CI` env var is set to true or the command output corresponds to a terminal, \
            \this option will be enabled by default."
    , flag' (Just WithoutColors) $
        long "no-color" <>
        help "Disable ANSI coloring of output."
    , pure Nothing
    ]
  oExclusionOptions <- exclusionOptionsParser
  oNetworkingOptions <- networkingOptionsParser
  oScanPolicy <- flag OnlyTracked IncludeUntracked $
    long "include-untracked" <>
    help "Scan and treat as existing files that were not added to Git.\
         \ Files explicitly ignored by Git are always ignored by xrefcheck."
  return Options{..}

exclusionOptionsParser :: Parser ExclusionOptions
exclusionOptionsParser = do
  eoIgnore <- many . globOption $
    long "ignore" <>
    metavar "GLOB PATTERN" <>
    help "Ignore these files. References to them will fail verification,\
         \ and references from them will not be verified.\
         \ Glob patterns that contain wildcards MUST be enclosed\
         \ in quotes to avoid being expanded by shell."
  return ExclusionOptions{..}

networkingOptionsParser :: Parser NetworkingOptions
networkingOptionsParser = do
  noMaxRetries <- option (Just <$> auto) $
    long "retries" <>
    metavar "INT" <>
    value Nothing <>
    help "How many attempts to retry an external link after getting \
         \a \"429 Too Many Requests\" response."
  return NetworkingOptions{..}

dumpConfigOptions :: Parser Command
dumpConfigOptions = hsubparser $
  command "dump-config" $
    info parser $
    progDesc "Dump default configuration into a file."
  where
    parser = DumpConfig <$> repoTypeOption <*> mode

    repoTypeOption =
      option repoTypeReadM $
      short 't' <>
      long "type" <>
      metavar "REPOSITORY TYPE" <>
      help [int||
      Git repository type. \
      Can be (#{intercalate " | " $ map show allFlavors}). \
      Case insensitive.
      |]

    mode =
      stdoutMode <|> fileMode

    fileMode =
      DCMFile <$> forceMode <*> outputOption

    stdoutMode =
      flag' DCMStdout $
      long "stdout" <>
      help "Write the config file to stdout."

    forceMode =
      switch $
      long "force" <>
      help "Overwrite the config file if it already exists."

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
ignoreModesMsg = header <> body
  where
    header = "To ignore a link in your markdown, \
             \include \"<!-- xrefcheck: ignore <mode> -->\"\n\
             \comment with one of these modes:\n"
    body = fillSep $ map formatDesc modeDescr

    modeDescr :: [(String, [String])]
    modeDescr =
      [ ("  \"link\"",      L.words "Ignore the link right after the comment.")
      , ("  \"paragraph\"", L.words "Ignore the whole paragraph after the comment.")
      , ("  \"file\"",      L.words "This mode can only be used at the top of \
                                    \markdown or right after comments at the top.")
      ]

    modeIndent = length ("\"paragraph\"" :: String) + 2
    descrIndent = 27 - modeIndent

    formatDesc (mode, descr) =
      fill modeIndent (pretty mode) <>
      indent descrIndent (fillSep $ map pretty descr)
