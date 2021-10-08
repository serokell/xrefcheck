{- SPDX-FileCopyrightText: 2018-2021 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

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

import Control.Concurrent.Async (wait, withAsync)
import Control.Exception (throwIO)
import Control.Monad.Except (MonadError (..))
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Metrics (damerauLevenshteinNorm)
import Data.Traversable (for)
import Fmt (Buildable (..), blockListF', listF, (+|), (|+))
import qualified GHC.Exts as Exts
import Network.FTP.Client
  (FTPException (..), FTPResponse (..), ResponseStatus (..), login, nlst, size, withFTP, withFTPS)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), responseStatus)
import Network.HTTP.Req
  (AllowsBody, CanHaveBody (NoBody), GET (..), HEAD (..), HttpBodyAllowed, HttpException (..),
  HttpMethod, NoReqBody (..), defaultHttpConfig, ignoreResponse, req, runReq, useURI)
import Network.HTTP.Types.Status (Status, statusCode, statusMessage)
import System.Console.Pretty (Style (..), style)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import qualified System.FilePath.Glob as Glob
import Text.Regex.TDFA.Text (Regex, regexec)
import Text.URI (Authority (..), URI (..), mkURI)
import Time (RatioNat, Second, Time (..), ms, threadDelay, timeout)

import Data.Bits (toIntegralSized)
import Xrefcheck.Config
import Xrefcheck.Core
import Xrefcheck.Orphans ()
import Xrefcheck.Progress
import Xrefcheck.System

{-# ANN module ("HLint: ignore Use uncurry" :: Text) #-}
{-# ANN module ("HLint: ignore Use 'runExceptT' from Universum" :: Text) #-}

-----------------------------------------------------------
-- General verification
-----------------------------------------------------------

newtype VerifyResult e = VerifyResult [e]
  deriving (Show, Eq, Functor)

deriving instance Semigroup (VerifyResult e)
deriving instance Monoid (VerifyResult e)

instance Buildable e => Buildable (VerifyResult e) where
  build vr = case verifyErrors vr of
    Nothing   -> "ok"
    Just errs -> listF errs

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
    "In file " +| style Faint (style Bold wrlFile) |+ "\nbad "
    +| wrlReference |+ "\n"
    +| wrlItem |+ "\n\n"

data VerifyError
  = LocalFileDoesNotExist FilePath
  | AnchorDoesNotExist Text [Anchor]
  | AmbiguousAnchorRef FilePath Text (NonEmpty Anchor)
  | ExternalResourceInvalidUri
  | ExternalResourceInvalidUrl (Maybe Text)
  | ExternalResourceUnknownProtocol
  | ExternalHttpResourceUnavailable Status
  | ExternalFtpResourceUnavailable FTPResponse
  | ExternalFtpException FTPException
  | FtpEntryDoesNotExist FilePath
  | ExternalResourceSomeError Text
  deriving (Show, Eq)

instance Buildable VerifyError where
  build = \case
    LocalFileDoesNotExist file ->
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
      "⛂  Invalid URI\n"

    ExternalResourceInvalidUrl Nothing ->
      "⛂  Invalid URL\n"

    ExternalResourceInvalidUrl (Just message) ->
      "⛂  Invalid URL (" +| message |+ ")\n"

    ExternalResourceUnknownProtocol ->
      "⛂  Bad url (expected 'http','https', 'ftp' or 'ftps')\n"

    ExternalHttpResourceUnavailable status ->
      "⛂  Resource unavailable (" +| statusCode status |+ " " +|
      decodeUtf8 @Text (statusMessage status) |+ ")\n"

    ExternalFtpResourceUnavailable response ->
      "⛂  Resource unavailable:\n" +| response |+ "\n"

    ExternalFtpException err ->
      "⛂  FTP exception (" +| err |+ ")\n"

    FtpEntryDoesNotExist entry ->
      "⛂ File or directory does not exist:\n" +| entry |+ "\n"

    ExternalResourceSomeError err ->
      "⛂  " +| build err |+ "\n\n"
    where
      anchorHints = \case
        []  -> "\n"
        [h] -> ",\n   did you mean " +| h |+ "?\n"
        hs  -> ", did you mean:\n" +| blockListF' "    -" build hs

data NeedsCaching key
  = NoCaching
  | CacheUnderKey key

-- | Perform concurrent traversal of the list with the caching mechanism.
-- The function is semantically similar to @Control.Concurrent.Async.forConcurrently@;
-- each asynchronous result of the @action@ is prepended to the accumulator list @[Async b]@.
-- Additionally, these action results may also be inserted in a map of the type
-- @Map cacheKey (Async b)@, depending on the return value of the function
-- @a -> NeedsCaching cacheKey@ applied to each of the element from the given list.
-- If an element of the type @a@ needs caching, and the value is already present in the map,
-- then the @action@ will not be executed, and the value is added to the accumulator list.
-- After the whole list has been traversed, the accumulator is traversed once again to ensure
-- every asynchronous action is completed.
forConcurrentlyCaching
  :: Ord cacheKey
  => [a] -> (a -> NeedsCaching cacheKey) -> (a -> IO b) -> IO [b]
forConcurrentlyCaching list needsCaching action = go [] M.empty list
  where
    go acc cached (x : xs) = case needsCaching x of
      NoCaching -> do
        withAsync (action x) $ \b ->
          go (b : acc) cached xs
      CacheUnderKey cacheKey -> do
        case M.lookup cacheKey cached of
          Nothing -> do
            withAsync (action x) $ \b ->
              go (b : acc) (M.insert cacheKey b cached) xs
          Just b -> go (b : acc) cached xs
    go acc _ [] = for acc wait <&> reverse

verifyRepo
  :: Rewrite
  -> VerifyConfig
  -> VerifyMode
  -> FilePath
  -> RepoInfo
  -> IO (VerifyResult $ WithReferenceLoc VerifyError)
verifyRepo
  rw
  config@VerifyConfig{..}
  mode
  root
  repoInfo'@(RepoInfo repoInfo)
    = do
  let toScan = do
        (file, fileInfo) <- M.toList repoInfo
        guard . not $ any ((`isPrefixOf` file) . (root </>)) vcNotScanned
        ref <- _fiReferences fileInfo
        return (file, ref)

  progressRef <- newIORef $ initVerifyProgress (map snd toScan)

  accumulated <- withAsync (printer progressRef) $ \_ ->
    forConcurrentlyCaching toScan ifExternalThenCache $ \(file, ref) ->
      verifyReference config mode progressRef repoInfo' root file ref
  return $ fold accumulated
  where
    printer progressRef = forever $ do
      readIORef progressRef >>= reprintAnalyseProgress rw mode
      threadDelay (ms 100)

    ifExternalThenCache (_, Reference{..}) = case locationType rLink of
      ExternalLoc -> CacheUnderKey rLink
      _           -> NoCaching

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
verifyReference
  config@VerifyConfig{..}
  mode
  progressRef
  (RepoInfo repoInfo)
  root
  fileWithReference
  ref@Reference{..}
    = do
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

    let moveProgress = incProgress .
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
        Just referredFileInfo -> whenJust mAnchor $
          checkAnchor referredFile (_fiAnchors referredFileInfo)

    checkReferredFileExists file = do
      let fileExists = readingSystem $ doesFileExist file
      let dirExists = readingSystem $ doesDirectoryExist file

      let cfile = readingSystem $ canonicalizePath file
      let isVirtual = or
            [ Glob.match pat cfile
            | virtualFile <- vcVirtualFiles
            , let pat = bindGlobPattern root virtualFile
            ]

      unless (fileExists || dirExists || isVirtual) $
        throwError (LocalFileDoesNotExist file)

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
  | skipCheck = return mempty
  | otherwise = fmap toVerifyRes $ runExceptT $ do
      uri <- mkURI link & maybe (throwError ExternalResourceInvalidUri) pure

      case toString <$> uriScheme uri of
        Just "http" -> checkHttp uri
        Just "https" -> checkHttp uri
        Just "ftp" -> checkFtp uri False
        Just "ftps" -> checkFtp uri True
        _ -> throwError ExternalResourceUnknownProtocol
  where
    skipCheck = isIgnored || (not vcCheckLocalhost && isLocalLink)
      where
        isIgnored =  maybe False (doesMatchAnyRegex link) vcIgnoreRefs
        isLocalLink = any (`T.isInfixOf` link) ["://localhost", "://127.0.0.1"]

    doesMatchAnyRegex :: Text -> ([Regex] -> Bool)
    doesMatchAnyRegex src = any $ \regex ->
      case regexec regex src of
        Right res -> case res of
          Just (before, match, after, _) ->
            null before && null after && not (null match)
          Nothing -> False
        Left _ -> False

    checkHttp :: URI -> ExceptT VerifyError IO ()
    checkHttp uri = makeHttpRequest uri HEAD 0.3 `catchError` \_ ->
      makeHttpRequest uri GET 0.7

    makeHttpRequest
      :: (HttpMethod method, HttpBodyAllowed (AllowsBody method) 'NoBody)
      => URI
      -> method
      -> RatioNat
      -> ExceptT VerifyError IO ()
    makeHttpRequest uri method timeoutFrac = do
      parsedUrl <- case useURI uri of
        -- accordingly to source code - Nothing can be only in case when
        -- protocol is not http or https, but we've checked it already
        -- so just in case we throw exception here
        Nothing -> throwError $ ExternalResourceInvalidUrl Nothing
        Just u -> pure u
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
      [ if vcIgnoreAuthFailures -- unauthorized access
        then flip elem [403, 401]
        else const False
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

    checkFtp :: URI -> Bool -> ExceptT VerifyError IO ()
    checkFtp uri secure = do
      -- get authority which stores host and port
      authority <- case uriAuthority uri of
        Right a -> pure a
        Left _ -> throwError $
          ExternalResourceInvalidUrl (Just "FTP path must be absolute")
      let host = toString $ authHost authority
      port :: Int <- case toIntegralSized . fromMaybe 21 $ authPort authority of
        Just p -> pure p
        Nothing -> throwError $
          ExternalResourceInvalidUrl (Just "Bad port")
      -- build path from pieces
      path <- case uriPath uri of
        Nothing -> pure ""
        Just (_, pieces) -> pure
          . mconcat
          . intersperse "/"
          . map toString
          . toList
          $ pieces
      makeFtpRequest host port path secure `catch` \e ->
        throwError $ ExternalFtpException e

    makeFtpRequest
      :: String
      -> Int
      -> FilePath
      -> Bool
      -> ExceptT VerifyError IO ()
    makeFtpRequest host port path secure = handler host port $
      \handle response -> do
        -- check connection status
        when (frStatus response /= Success) $
          throwError $ ExternalFtpResourceUnavailable response
        -- anonymous login
        loginResp <- login handle "anonymous" ""
        -- check login status
        when (frStatus loginResp /= Success) $
          if vcIgnoreAuthFailures
          then pure ()
          else throwError $ ExternalFtpException $ UnsuccessfulException loginResp
        -- If the response is non-null, the path is definitely a directory;
        -- If the response is null, the path may be a file or may not exist.
        dirList <- nlst handle [ "-a", path ]
        when (BS.null dirList) $ do
          -- The server-PI will respond to the SIZE command with a 213 reply
          -- giving the transfer size of the file whose pathname was supplied,
          -- or an error response if the file does not exist, the size is
          -- unavailable, or some other error has occurred.
          _ <- size handle path `catch` \case
              UnsuccessfulException _ -> throwError $ FtpEntryDoesNotExist path
              FailureException FTPResponse{..} | frCode == 550 ->
                throwError $ FtpEntryDoesNotExist path
              err -> liftIO $ throwIO err
          pure ()
      where
        handler = if secure then withFTPS else withFTP
