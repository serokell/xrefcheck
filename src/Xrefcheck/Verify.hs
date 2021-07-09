{- SPDX-FileCopyrightText: 2018-2021 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Xrefcheck.Verify
    ( -- * General verification
      VerifyResult (..)
    , verifyOk
    , verifyErrors
    , verifying

    , WithReferenceLoc (..)

      -- * Cross-references validation
    , VerifyError (..)
    , verifyRepo
    , checkExternalResource
    ) where

import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as M
import qualified Data.Text as T
import qualified GHC.Exts as Exts
import qualified System.FilePath.Glob as Glob

import Control.Concurrent.Async (forConcurrently, withAsync)
import Control.Monad.Except (MonadError (..))
import Data.Maybe (fromJust)
import Data.Text.Metrics (damerauLevenshteinNorm)
import Fmt (Buildable (..), blockListF', listF, (+|), (|+))
import Network.FTP.Client
  (FTPMessage (..), FTPResponse (..), ResponseStatus (..), login, nlst, withFTP)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), responseStatus)
import Network.HTTP.Req
  (AllowsBody, CanHaveBody (NoBody), GET (..), HEAD (..), HttpBodyAllowed, HttpException (..),
  HttpMethod, NoReqBody (..), defaultHttpConfig, ignoreResponse, req, runReq, useURI)
import Network.HTTP.Types.Status (Status, statusCode, statusMessage)
import System.Console.Pretty (Style (..), style)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import Text.Regex.TDFA.Text (Regex, regexec)
import Text.URI (Authority (..), URI (..), mkURI, unRText)
import Time (RatioNat, Second, Time (..), ms, threadDelay, timeout)

import Xrefcheck.Config
import Xrefcheck.Core
import Xrefcheck.Progress
import Xrefcheck.System

{-# ANN module ("HLint: ignore Use uncurry" :: Text) #-}
{-# ANN module ("HLint: ignore Use 'runExceptT' from Universum" :: Text) #-}

-----------------------------------------------------------
-- General verification
-----------------------------------------------------------

newtype VerifyResult e = VerifyResult [e]
    deriving (Show, Functor)

deriving instance Semigroup (VerifyResult e)
deriving instance Monoid (VerifyResult e)

instance Buildable e => Buildable (VerifyResult e) where
    build vr = maybe "ok" listF $ verifyErrors vr

verifyOk :: VerifyResult e -> Bool
verifyOk (VerifyResult errors) = null errors

verifyErrors :: VerifyResult e -> Maybe (NonEmpty e)
verifyErrors (VerifyResult errors) = nonEmpty errors

verifying :: Monad m => ExceptT e m () -> m (VerifyResult e)
verifying (ExceptT action) = fmap toVerifyRes action

toVerifyRes :: Either e () -> VerifyResult e
toVerifyRes = VerifyResult . either one (\() -> [])

-----------------------------------------------------------
-- Cross-references validation
-----------------------------------------------------------

data WithReferenceLoc a = WithReferenceLoc
    { wrlFile      :: FilePath
    , wrlReference :: Reference
    , wrlItem      :: a
    }

instance Buildable a => Buildable (WithReferenceLoc a) where
    build WithReferenceLoc{..} =
        "In file " +| style Faint (style Bold wrlFile) |+ "\nbad " +| wrlReference |+ "\n"
        +| wrlItem |+ "\n\n"

data VerifyError
    = FileDoesNotExist FilePath
    | AnchorDoesNotExist Text [Anchor]
    | AmbiguousAnchorRef FilePath Text (NonEmpty Anchor)
    | ExternalResourceInvalidUri
    | ExternalResourceUnknownProtocol
    | ExternalHttpResourceUnavailable Status
    | ExternalFtpResourceUnavailable FTPResponse
    | ExternalResourceSomeError Text
    deriving (Show)

instance Buildable VerifyError where
    build = \case
        FileDoesNotExist file ->
            "⛀  File does not exist:\n   " +| file |+ "\n"
        AnchorDoesNotExist anchor similar ->
            "⛀  Anchor '" +| anchor |+ "' is not present" +|
            anchorHints similar
        AmbiguousAnchorRef file anchor fileAnchors ->
            "⛀  Ambiguous reference to anchor '" +| anchor |+ "'\n   " +|
            "In file " +| file |+ "\n   " +|
            "Similar anchors are:\n" +|
                blockListF' "    -" build fileAnchors |+ "" +|
            "   Use of such anchors is discouraged because referenced object\n\
            \   can change silently whereas the document containing it evolves.\n"
        ExternalResourceInvalidUri ->
            "⛂  Invalid url\n"
        ExternalResourceUnknownProtocol ->
            "⛂  Bad url (expected 'http','https' or 'ftp')\n"
        ExternalHttpResourceUnavailable status ->
            "⛂  Resource unavailable (" +| statusCode status |+ " " +|
            decodeUtf8 @Text (statusMessage status) |+ ")\n"
        ExternalFtpResourceUnavailable status ->
            "⛂  Resource unavailable (" +| frStatus status |+ " " +|
            decodeUtf8 @Text (
              case frMessage status of
                SingleLine s -> s
                MultiLine ss -> BS.concat ss
            ) |+ ")\n"
        ExternalResourceSomeError err ->
            "⛂  " +| build err |+ "\n\n"
      where
        anchorHints = \case
            []  -> "\n"
            [h] -> ",\n   did you mean " +| h |+ "?\n"
            hs  -> ", did you mean:\n" +| blockListF' "    -" build hs

instance Buildable ResponseStatus where
  build = show

verifyRepo
    :: Rewrite
    -> VerifyConfig
    -> VerifyMode
    -> FilePath
    -> RepoInfo
    -> IO (VerifyResult $ WithReferenceLoc VerifyError)
verifyRepo rw config@VerifyConfig{..} mode root repoInfo'@(RepoInfo repoInfo) = do
    let toScan = do
          (file, fileInfo) <- M.toList repoInfo
          guard . not $ any ((`isPrefixOf` file) . (root </>)) vcNotScanned
          ref <- _fiReferences fileInfo
          return (file, ref)

    progressRef <- newIORef $ initVerifyProgress (map snd toScan)

    withAsync (printer progressRef) $ \_ ->
        fmap fold . forConcurrently toScan $ \(file, ref) ->
            verifyReference config mode progressRef repoInfo' root file ref
  where
    printer progressRef = forever $ do
        readIORef progressRef >>= reprintAnalyseProgress rw mode
        threadDelay (ms 100)

shouldCheckLocType :: VerifyMode -> LocationType -> Bool
shouldCheckLocType mode locType
    | isExternal locType = shouldCheckExternal mode
    | isLocal locType = shouldCheckLocal mode
    | otherwise = False

verifyReference
    :: VerifyConfig
    -> VerifyMode
    -> IORef VerifyProgress
    -> RepoInfo
    -> FilePath
    -> FilePath
    -> Reference
    -> IO (VerifyResult $ WithReferenceLoc VerifyError)
verifyReference config@VerifyConfig{..} mode progressRef (RepoInfo repoInfo)
                root fileWithReference ref@Reference{..} = do

    let locType = locationType rLink

    if shouldCheckLocType mode locType
    then do
        res <- case locType of
            LocalLoc    -> checkRef rAnchor fileWithReference
            RelativeLoc -> checkRef rAnchor
                          (takeDirectory fileWithReference
                            </> toString (canonizeLocalRef rLink))
            AbsoluteLoc -> checkRef rAnchor (root <> toString rLink)
            ExternalLoc -> checkExternalResource config rLink
            OtherLoc    -> verifying pass

        let moveProgress =
                incProgress .
                (if verifyOk res then id else incProgressErrors)

        atomicModifyIORef' progressRef $ \VerifyProgress{..} ->
            ( if isExternal locType
              then VerifyProgress{ vrExternal = moveProgress vrExternal, .. }
              else VerifyProgress{ vrLocal = moveProgress vrLocal, .. }
            , ()
            )
        return $ fmap (WithReferenceLoc fileWithReference ref) res
    else return mempty
  where
    checkRef mAnchor referredFile = verifying $ do
        checkReferredFileExists referredFile
        case M.lookup referredFile repoInfo of
            Nothing -> pass  -- no support for such file, can do nothing
            Just referredFileInfo ->
                whenJust mAnchor $ checkAnchor referredFile (_fiAnchors referredFileInfo)

    checkReferredFileExists file = do
        let fileExists = readingSystem $ doesFileExist file
        let dirExists = readingSystem $ doesDirectoryExist file

        let cfile = readingSystem $ canonicalizePath file
        let isVirtual = or
                [ Glob.match pat cfile
                | virtualFile <- vcVirtualFiles
                , let pat = bindGlobPattern root virtualFile ]

        unless (fileExists || dirExists || isVirtual) $
            throwError (FileDoesNotExist file)

    checkAnchor file fileAnchors anchor = do
        checkAnchorReferenceAmbiguity file fileAnchors anchor
        checkDeduplicatedAnchorReference file fileAnchors anchor
        checkAnchorExists fileAnchors anchor

    -- Detect a case when original file contains two identical anchors, github
    -- has added a suffix to the duplicate, and now the original is referrenced -
    -- such links are pretty fragile and we discourage their use despite
    -- they are in fact unambiguous.
    checkAnchorReferenceAmbiguity file fileAnchors anchor = do
        let similarAnchors = filter ((== anchor) . aName) fileAnchors
        when (length similarAnchors > 1) $
            throwError $ AmbiguousAnchorRef file anchor (Exts.fromList similarAnchors)

    -- Similar to the previous one, but for the case when we reference the
    -- renamed duplicate.
    checkDeduplicatedAnchorReference file fileAnchors anchor =
        whenJust (stripAnchorDupNo anchor) $ \origAnchor ->
            checkAnchorReferenceAmbiguity file fileAnchors origAnchor

    checkAnchorExists givenAnchors anchor =
        case find ((== anchor) . aName) givenAnchors of
            Just _ -> pass
            Nothing ->
                let isSimilar = (>= vcAnchorSimilarityThreshold)
                    similarAnchors =
                        filter (isSimilar . realToFrac . damerauLevenshteinNorm anchor . aName)
                        givenAnchors
                in throwError $ AnchorDoesNotExist anchor similarAnchors

checkExternalResource :: VerifyConfig -> Text -> IO (VerifyResult VerifyError)
checkExternalResource VerifyConfig{..} link
    | isIgnored = return mempty
    | doesReferLocalhost = return mempty
    | otherwise = fmap toVerifyRes $ runExceptT $ do
        uri <- mkURI link & maybe (throwError ExternalResourceInvalidUri) pure

        case unRText <$> uriScheme uri of
          Just "http" -> checkHttp uri
          Just "https" -> checkHttp uri
          Just "ftp" -> checkFtp uri
          _ -> throwError ExternalResourceUnknownProtocol
  where
    isIgnored =
        let maybeIsIgnored = doesMatchAnyRegex link <$> vcIgnoreRefs
        in fromMaybe False maybeIsIgnored

    doesReferLocalhost = any (`T.isInfixOf` link) ["://localhost", "://127.0.0.1"]

    doesMatchAnyRegex :: Text -> ([Regex] -> Bool)
    doesMatchAnyRegex src = any $ \regex ->
        case regexec regex src of
            Right res -> case res of
                Just (before, match, after, _) -> null before && null after && not (null match)
                Nothing -> False
            Left _ -> False

    checkHttp :: URI -> ExceptT VerifyError IO ()
    checkHttp uri = do
      res <- liftIO $ runExceptT $ makeHttpRequest uri HEAD 0.3
      case res of
        Right () -> return ()
        Left   _ -> makeHttpRequest uri GET 0.7

    makeHttpRequest
      :: (HttpMethod method, HttpBodyAllowed (AllowsBody method) 'NoBody)
      => URI
      -> method
      -> RatioNat
      -> ExceptT VerifyError IO ()
    makeHttpRequest uri method timeoutFrac = do
        parsedUrl <- useURI uri
                   & maybe (throwError ExternalResourceUnknownProtocol) pure
        let reqLink = case parsedUrl of
                Left (url, option) ->
                    runReq defaultHttpConfig $
                    req method url NoReqBody ignoreResponse option
                Right (url, option) ->
                    runReq defaultHttpConfig $
                    req method url NoReqBody ignoreResponse option

        let maxTime = Time @Second $ unTime vcExternalRefCheckTimeout * timeoutFrac

        mres <- liftIO (timeout maxTime $ void reqLink) `catch`
          (either throwError (\() -> return (Just ())) . interpretErrors)
        maybe (throwError $ ExternalResourceSomeError "Response timeout") pure mres

    isAllowedErrorCode = or . sequence
        -- We have to stay conservative - if some URL can be accessed under
        -- some circumstances, we should do our best to report it as fine.
        [ (403 ==)  -- unauthorized access
        , (405 ==)  -- method mismatch
        ]

    interpretErrors = \case
        JsonHttpException _ -> error "External link JSON parse exception"
        VanillaHttpException err -> case err of
            InvalidUrlException{} -> error "External link URL invalid exception"
            HttpExceptionRequest _ exc -> case exc of
                StatusCodeException resp _
                    | isAllowedErrorCode (statusCode $ responseStatus resp) -> Right ()
                    | otherwise -> Left $ ExternalHttpResourceUnavailable (responseStatus resp)
                other -> Left . ExternalResourceSomeError $ show other

    checkFtp :: URI -> ExceptT VerifyError IO ()
    checkFtp uri = do
      -- get authority which stores host and port
      authority <- case uriAuthority uri of
        Right a -> pure a
        Left _ -> throwError ExternalResourceInvalidUri
      let host = T.unpack . unRText $ authHost authority
      let port :: Int = fromIntegral . fromMaybe 21 $ authPort authority
      let pieces = uriPath uri
      -- in case there aro no pieces - smth wrong with url
      unless (isJust pieces) $
        throwError ExternalResourceInvalidUri
      let (trailing, pathList) = fromJust pieces
      -- in case there are trailing slash - smth wrong with url
      when trailing $
        throwError ExternalResourceInvalidUri
      -- build path from pieces
      let path = T.unpack . mconcat
            . NonEmpty.toList
            . NonEmpty.intersperse "/"
            . NonEmpty.map unRText
            $ pathList
      makeFtpRequest host port path

    makeFtpRequest :: String -> Int -> FilePath -> ExceptT VerifyError IO ()
    makeFtpRequest host port path = withFTP host port $
      \handle resp -> do
        -- check connection status
        when (frStatus resp /= Success) $
          throwError $ ExternalFtpResourceUnavailable resp
        -- anonymous login
        loginResp <- login handle "anonymous" ""
        -- check login status
        when (frStatus loginResp /= Success) $
          throwError $ ExternalFtpResourceUnavailable loginResp
        file <- nlst handle [ path ]
        -- check file exists
        when (BS.null file) $
          throwError $ FileDoesNotExist path
