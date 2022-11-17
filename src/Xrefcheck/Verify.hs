{- SPDX-FileCopyrightText: 2018-2021 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Xrefcheck.Verify
  ( -- * General verification
    VerifyResult (..)
  , verifyOk
  , verifyErrors
  , verifying

  , RetryAfter (..)
  , WithReferenceLoc (..)

    -- * Concurrent traversal with caching
  , NeedsCaching (..)
  , forConcurrentlyCaching

    -- * Cross-references validation
  , VerifyError (..)
  , verifyRepo
  , verifyReference
  , checkExternalResource

    -- * URI parsing
  , parseUri
  , reportVerifyErrs
  ) where

import Universum

import Control.Concurrent.Async (async, cancel, wait, withAsync, Async, poll)
import Control.Exception (AsyncException (..), throwIO)
import Control.Monad.Except (MonadError (..))
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.Map qualified as M
import Data.Reflection (Given)
import Data.Text.Metrics (damerauLevenshteinNorm)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, readPTime, rfc822DateFormat)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Traversable (for)
import Fmt (Buildable (..), fmt, maybeF, nameF)
import GHC.Exts qualified as Exts
import GHC.Read (Read (readPrec))
import Network.FTP.Client
  (FTPException (..), FTPResponse (..), ResponseStatus (..), login, nlst, size, withFTP, withFTPS)
import Network.HTTP.Client
  (HttpException (..), HttpExceptionContent (..), Response, responseHeaders, responseStatus)
import Network.HTTP.Req
  (AllowsBody, CanHaveBody (NoBody), GET (..), HEAD (..), HttpBodyAllowed, HttpException (..),
  HttpMethod, NoReqBody (..), defaultHttpConfig, ignoreResponse, req, runReq, useURI)
import Network.HTTP.Types.Header (hRetryAfter)
import Network.HTTP.Types.Status (Status, statusCode, statusMessage)
import System.FilePath
  (equalFilePath, joinPath, makeRelative, normalise, splitDirectories, takeDirectory, (</>))
import Text.Interpolation.Nyan
import Text.ParserCombinators.ReadPrec qualified as ReadPrec (lift)
import Text.Regex.TDFA.Text (Regex, regexec)
import Text.URI (Authority (..), ParseExceptionBs, URI (..), mkURIBs)
import Time (RatioNat, Second, Time (..), ms, sec, threadDelay, timeout, (+:+), (-:-))
import URI.ByteString qualified as URIBS

import Data.Bits (toIntegralSized)
import Xrefcheck.Config
import Xrefcheck.Core
import Xrefcheck.Orphans ()
import Xrefcheck.Progress
import Xrefcheck.Scan
import Xrefcheck.System
import Xrefcheck.Util
import Control.Exception.Safe (handleAsync, handleJust)

{-# ANN module ("HLint: ignore Use uncurry" :: Text) #-}
{-# ANN module ("HLint: ignore Use 'runExceptT' from Universum" :: Text) #-}

-----------------------------------------------------------
-- General verification
-----------------------------------------------------------

newtype VerifyResult e = VerifyResult [e]
  deriving newtype (Show, Eq, Functor)

deriving newtype instance Semigroup (VerifyResult e)
deriving newtype instance Monoid (VerifyResult e)

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

instance (Given ColorMode, Buildable a) => Buildable (WithReferenceLoc a) where
  build WithReferenceLoc{..} = [int||
    In file #{styleIfNeeded Faint (styleIfNeeded Bold wrlFile)}
    bad #{wrlReference}
    #{wrlItem}
    |]

data VerifyError
  = LocalFileDoesNotExist FilePath
  | LocalFileOutsideRepo FilePath
  | LinkTargetNotAddedToGit FilePath
  | AnchorDoesNotExist Text [Anchor]
  | AmbiguousAnchorRef FilePath Text (NonEmpty Anchor)
  | ExternalResourceInvalidUri URIBS.URIParseError
  | ExternalResourceUriConversionError ParseExceptionBs
  | ExternalResourceInvalidUrl (Maybe Text)
  | ExternalResourceUnknownProtocol
  | ExternalHttpResourceUnavailable Status
  | ExternalHttpTooManyRequests (Maybe RetryAfter)
  | ExternalFtpResourceUnavailable FTPResponse
  | ExternalFtpException FTPException
  | FtpEntryDoesNotExist FilePath
  | ExternalResourceSomeError Text
  deriving stock (Show, Eq)

instance Given ColorMode => Buildable VerifyError where
  build = \case
    LocalFileDoesNotExist file ->
      [int||
      ⛀  File does not exist:
         #{file}
      |]

    LocalFileOutsideRepo file ->
      [int||
      ⛀  Link targets a local file outside repository:
         #{file}
      |]


    LinkTargetNotAddedToGit file ->
      [int||
      ⛀  Link target is not tracked by Git:
         #{file}
         Please run "git add" before running xrefcheck or enable --include-untracked CLI option.
      |]

    AnchorDoesNotExist anchor similar -> case nonEmpty similar of
      Nothing ->
        [int||
        ⛀  Anchor '#{anchor}' is not present
        |]
      Just otherAnchors ->
        [int||
        ⛀  Anchor '#{anchor}' is not present, did you mean:
        #{interpolateIndentF 4 $ interpolateBlockListF otherAnchors}
        |]

    AmbiguousAnchorRef file anchor fileAnchors ->
      [int||
      ⛀  Ambiguous reference to anchor '#{anchor}'
        In file #{file}
        It could refer to either:
      #{interpolateIndentF 4 $ interpolateBlockListF fileAnchors}
        Use of ambiguous anchors is discouraged because the target
        can change silently while the document containing it evolves.
      |]

    ExternalResourceInvalidUri err ->
      [int||
      ⛂  Invalid URI (#{err})
      |]

    ExternalResourceUriConversionError err ->
      [int||
      ⛂  Invalid URI
      #{interpolateIndentF 4 . build $ displayException err}
      |]

    ExternalResourceInvalidUrl Nothing ->
      [int||
      ⛂  Invalid URL
      |]

    ExternalResourceInvalidUrl (Just message) ->
      [int||
      ⛂  Invalid URL (#{message})
      |]

    ExternalResourceUnknownProtocol ->
      [int||
      ⛂  Bad url (expected 'http','https', 'ftp' or 'ftps')
      |]

    ExternalHttpResourceUnavailable status ->
      [int||
      ⛂  Resource unavailable (#{statusCode status} #{decodeUtf8 @Text (statusMessage status)})
      |]

    ExternalHttpTooManyRequests retryAfter ->
      [int||
      ⛂  Resource unavailable (429 Too Many Requests; retry after #{maybeF retryAfter})
      |]

    ExternalFtpResourceUnavailable response ->
      [int||
      ⛂  Resource unavailable:
      #{response}
      |]

    ExternalFtpException err ->
      [int||
      ⛂  FTP exception (#{err})
      |]

    FtpEntryDoesNotExist entry ->
      [int||
      ⛂ File or directory does not exist:
      #{entry}
      |]

    ExternalResourceSomeError err ->
      [int||
      ⛂  #{err}
      |]

reportVerifyErrs
  :: Given ColorMode => NonEmpty (WithReferenceLoc VerifyError) -> IO ()
reportVerifyErrs errs = fmt
  [int||
  === Invalid references found ===

  #{interpolateIndentF 2 (interpolateBlockListF' "➥ " build errs)}
  Invalid references dumped, #{length errs} in total.
  |]


data RetryAfter = Date UTCTime | Seconds (Time Second)
  deriving stock (Show, Eq)

instance Read RetryAfter where
  readPrec = asum
    [ ReadPrec.lift $ Date <$> readPTime True defaultTimeLocale rfc822DateFormat
    , readPrec @Natural <&> Seconds . sec . fromIntegral @_ @RatioNat
    ]

instance Buildable RetryAfter where
  build (Date d) = nameF "date" $
    fromString $ formatTime defaultTimeLocale rfc822DateFormat d
  build (Seconds s) = nameF "seconds" $ show s

-- | Determine whether the verification result contains a fixable error.
isFixable :: VerifyError -> Bool
isFixable (ExternalHttpTooManyRequests _) = True
isFixable _ = False

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
-- If interrupted by AsyncException, returns this exception and list of already calcualted results
-- (their subset can be arbitrary). Computations that were not ended till this moment are cancelled.
forConcurrentlyCaching
  :: forall a b cacheKey.  Ord cacheKey
  => [a] -> (a -> NeedsCaching cacheKey) -> (a -> IO b) -> IO (Either (AsyncException, [b]) [b])
forConcurrentlyCaching list needsCaching action = go [] M.empty list
  where
    go
      :: [Async b]
      -> Map cacheKey (Async b)
      -> [a]
      -> IO (Either (AsyncException, [b]) [b])
    go acc cached items =
      case items of

        (x : xs) -> case needsCaching x of
          NoCaching -> do
            withAsync (action x) $ \b ->
              go (b : acc) cached xs
          CacheUnderKey cacheKey -> do
            case M.lookup cacheKey cached of
              Nothing -> do
                withAsync (action x) $ \b ->
                  go (b : acc) (M.insert cacheKey b cached) xs
              Just b -> go (b : acc) cached xs

        [] -> handleAsync
        -- Wait for all children threads to complete.
        --
        -- If, while the threads are running, the user hits Ctrl+C,
        -- a `UserInterrupt :: AsyncException` will be thrown onto the main thread.
        -- We catch it here, cancel all child threads,
        -- and return the results of only the threads that finished successfully.
          (\case
            UserInterrupt -> do
              partialResults <- for acc \asyncAction -> do
                cancel asyncAction
                poll asyncAction <&> \case
                  Just (Right a)  -> Just a
                  Just (Left _ex) -> Nothing
                  Nothing         -> Nothing
              pure $ Left (UserInterrupt, catMaybes partialResults)
            otherAsyncEx -> throwM otherAsyncEx
          )
          $ Right . reverse <$> for acc wait
      -- If action was already completed, then @cancel@ will have no effect, and we
      -- will get result from @cancel f >> poll f@. Otherwise action will be interrupted,
      -- so poll will return @Left (SomeException AsyncCancelled)@

verifyRepo
  :: Given ColorMode
  => Rewrite
  -> Config
  -> VerifyMode
  -> FilePath
  -> RepoInfo
  -> IO (VerifyResult $ WithReferenceLoc VerifyError)
verifyRepo
  rw
  config@Config{..}
  mode
  root
  repoInfo'@(RepoInfo files _)
    = do
  let toScan = do
        (file, fileInfo) <- M.toList files
        guard . not $ matchesGlobPatterns root (ecIgnoreRefsFrom cExclusions) file
        case fileInfo of
          Scanned fi -> do
            ref <- _fiReferences fi
            return (file, ref)
          NotScannable -> empty -- No support for such file, can do nothing.
          NotAddedToGit -> empty -- If this file is scannable, we've notified
                                 -- user that we are scanning only files
                                 -- added to Git while gathering RepoInfo.

  progressRef <- newIORef $ initVerifyProgress (map snd toScan)

  accumulated <- loopAsyncUntil (printer progressRef) do
    forConcurrentlyCaching toScan ifExternalThenCache $ \(file, ref) ->
      verifyReference config mode progressRef repoInfo' root file ref
  case accumulated of
    Right res -> return $ fold res
    Left (exception, partialRes) -> do
      -- The user has hit Ctrl+C; display any verification errors we managed to find and exit.
      let errs = verifyErrors (fold partialRes)
          total = length toScan
          checked = length partialRes
      whenJust errs $ reportVerifyErrs
      fmt [int|A|
          Interrupted (#s{exception}), checked #{checked} out of #{total} references.
          |]
      exitFailure

  where
    printer :: IORef VerifyProgress -> IO ()
    printer progressRef = do
      posixTime <- getPOSIXTime <&> posixTimeToTimeSecond
      progress <- atomicModifyIORef' progressRef $ \VerifyProgress{..} ->
        let prog = VerifyProgress{ vrExternal =
          checkTaskTimestamp posixTime vrExternal
                                 , ..
                                 }
        in (prog, prog)
      reprintAnalyseProgress rw mode posixTime progress
      -- Slight pause so we're not refreshing the progress bar more often than needed.
      threadDelay (ms 100)

    ifExternalThenCache :: (a, Reference) -> NeedsCaching Text
    ifExternalThenCache (_, Reference{..}) = case locationType rLink of
      ExternalLoc -> CacheUnderKey rLink
      _           -> NoCaching

shouldCheckLocType :: VerifyMode -> LocationType -> Bool
shouldCheckLocType mode locType
  | isExternal locType = shouldCheckExternal mode
  | isLocal locType = shouldCheckLocal mode
  | otherwise = False

verifyReference
  :: Config
  -> VerifyMode
  -> IORef VerifyProgress
  -> RepoInfo
  -> FilePath
  -> FilePath
  -> Reference
  -> IO (VerifyResult $ WithReferenceLoc VerifyError)
verifyReference
  config@Config{..}
  mode
  progressRef
  (RepoInfo files dirs)
  root
  fileWithReference
  ref@Reference{..}
    = retryVerification 0 $ do
        let locType = locationType rLink
        if shouldCheckLocType mode locType
        then case locType of
          CurrentFileLoc    -> checkRef rAnchor fileWithReference
          RelativeLoc       -> checkRef rAnchor
                                (normalise $ takeDirectory fileWithReference
                                  </> toString (canonizeLocalRef rLink))
          AbsoluteLoc       -> checkRef rAnchor (root <> toString rLink)
          ExternalLoc       -> checkExternalResource config rLink
          OtherLoc          -> verifying pass
        else return mempty
  where
    retryVerification
      :: Int
      -> IO (VerifyResult VerifyError)
      -> IO (VerifyResult $ WithReferenceLoc VerifyError)
    retryVerification numberOfRetries resIO = do
      res@(VerifyResult ves) <- resIO

      now <- getPOSIXTime <&> posixTimeToTimeSecond

      let toSeconds = \case
            Seconds s -> s
            -- Calculates the seconds left until @Retry-After@ date.
            -- Defaults to 0 if the date has already passed.
            Date date | utcTimeToTimeSecond date >= now -> utcTimeToTimeSecond date -:- now
            _ -> sec 0

      let toRetry = any isFixable ves && numberOfRetries < ncMaxRetries cNetworking
          currentRetryAfter = fromMaybe (ncDefaultRetryAfter cNetworking) $
            extractRetryAfterInfo res <&> toSeconds

      let moveProgress = alterOverallProgress numberOfRetries
                       . alterProgressErrors res numberOfRetries

      atomicModifyIORef' progressRef $ \VerifyProgress{..} ->
        ( if isExternal $ locationType rLink
          then VerifyProgress{ vrExternal =
            let vrExternalAdvanced = moveProgress vrExternal
            in if toRetry
               then case pTaskTimestamp vrExternal of
                      Just (TaskTimestamp ttc start)
                        | currentRetryAfter +:+ now <= ttc +:+ start -> vrExternalAdvanced
                      _ -> setTaskTimestamp currentRetryAfter now vrExternalAdvanced
               else vrExternalAdvanced, .. }
          else VerifyProgress{ vrLocal = moveProgress vrLocal, .. }
        , ()
        )
      if toRetry
      then do
        threadDelay currentRetryAfter
        retryVerification (numberOfRetries + 1) resIO
      else return $ fmap (WithReferenceLoc fileWithReference ref) res

    alterOverallProgress
      :: (Num a)
      => Int
      -> Progress a
      -> Progress a
    alterOverallProgress retryNumber
      | retryNumber > 0 = id
      | otherwise = incProgress

    alterProgressErrors
      :: (Num a)
      => VerifyResult VerifyError
      -> Int
      -> Progress a
      -> Progress a
    alterProgressErrors res@(VerifyResult ves) retryNumber
      | (ncMaxRetries cNetworking) == 0 =
          if ok then id
          else incProgressUnfixableErrors
      | retryNumber == 0 =
          if ok then id
          else if fixable then incProgressFixableErrors
          else incProgressUnfixableErrors
      | retryNumber == (ncMaxRetries cNetworking) =
          if ok then decProgressFixableErrors
          else fixableToUnfixable
      -- 0 < retryNumber < ncMaxRetries
      | otherwise =
          if ok then decProgressFixableErrors
          else if fixable then id
          else fixableToUnfixable
      where
        ok = verifyOk res
        fixable = any isFixable ves

    extractRetryAfterInfo :: VerifyResult VerifyError -> Maybe RetryAfter
    extractRetryAfterInfo = \case
      VerifyResult [ExternalHttpTooManyRequests retryAfter] -> retryAfter
      _ -> Nothing

    isVirtual = matchesGlobPatterns root (ecIgnoreLocalRefsTo cExclusions)

    checkRef mAnchor referredFile = verifying $
      unless (isVirtual referredFile) do
        checkReferredFileIsInsideRepo referredFile
        mFileStatus <- tryGetFileStatus referredFile
        case mFileStatus of
          Right (Scanned referredFileInfo) -> whenJust mAnchor $
            checkAnchor referredFile (_fiAnchors referredFileInfo)
          Right NotScannable -> pass  -- no support for such file, can do nothing
          Right NotAddedToGit -> throwError (LinkTargetNotAddedToGit referredFile)
          Left UntrackedDirectory -> throwError (LinkTargetNotAddedToGit referredFile)
          Left TrackedDirectory -> pass -- path leads to directory, currently
                                        -- if such link contain anchor, we ignore it

    -- expands ".." and "."
    -- expandIndirections "a/b/../c"      = "a/c"
    -- expandIndirections "a/b/c/../../d" = "a/d"
    -- expandIndirections "../../a"       = "../../a"
    -- expandIndirections "a/./b"         = "a/b"
    -- expandIndirections "a/b/./../c"    = "a/c"
    expandIndirections :: FilePath -> FilePath
    expandIndirections = joinPath . reverse . expand 0 . reverse . splitDirectories
      where
        expand :: Int -> [FilePath] -> [FilePath]
        expand acc ("..":xs) = expand (acc+1) xs
        expand acc (".":xs)  = expand acc xs
        expand 0 (x:xs)      = x : expand 0 xs
        expand acc (_:xs)    = expand (acc-1) xs
        expand acc []        = replicate acc ".."

    checkReferredFileIsInsideRepo file = unless
      (noNegativeNesting $ makeRelative root file) $
        throwError (LocalFileOutsideRepo file)
      where
        -- checks that relative filepath fully belongs to the root directory
        -- noNegativeNesting "a/../b" = True
        -- noNegativeNesting "a/../../b" = False
        noNegativeNesting path = all (>= 0) $ scanl
          (\n dir -> n + nestingChange dir)
          (0 :: Integer)
          $ splitDirectories path

        nestingChange ".." = -1
        nestingChange "." = 0
        nestingChange _ = 1

    -- Returns `Nothing` when path corresponds to an existing (and tracked) directory
    tryGetFileStatus :: FilePath -> ExceptT VerifyError IO (Either DirectoryStatus FileStatus)
    tryGetFileStatus file
      | Just f <- mFile = return $ Right f
      | Just d <- mDir = return $ Left d
      | otherwise = throwError (LocalFileDoesNotExist file)
      where
        matchesFilePath :: FilePath -> Bool
        matchesFilePath = equalFilePath $ expandIndirections file

        mFile :: Maybe FileStatus
        mFile = (files M.!) <$> find matchesFilePath (M.keys files)

        mDir :: Maybe DirectoryStatus
        mDir = (dirs M.!) <$> find matchesFilePath (M.keys dirs)

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
          let isSimilar = (>= scAnchorSimilarityThreshold cScanners)
              similarAnchors =
                filter (isSimilar . realToFrac . damerauLevenshteinNorm anchor . aName)
                givenAnchors
          in throwError $ AnchorDoesNotExist anchor similarAnchors

-- | Parse URI according to RFC 3986 extended by allowing non-encoded
-- `[` and `]` in query string.
parseUri :: Text -> ExceptT VerifyError IO URI
parseUri link = do
      -- There exist two main standards of URL parsing: RFC 3986 and the Web
      -- Hypertext Application Technology Working Group's URL standard. Ideally,
      -- we want to be able to parse the URLs in accordance with the latter
      -- standard, because it provides a much less ambiguous set of rules for
      -- percent-encoding special characters, and is essentially a living
      -- standard that gets updated constantly.
      --
      -- We have chosen the 'uri-bytestring' library for URI parsing because
      -- of the 'laxURIParseOptions' parsing configuration. 'mkURI' from
      -- the 'modern-uri' library parses URIs in accordance with RFC 3986 and does
      -- not provide a means of parsing customization, which contrasts with
      -- 'parseURI' that accepts a 'URIParserOptions'. One of the predefined
      -- configurations of this type is 'strictURIParserOptions', which follows
      -- RFC 3986, and the other -- 'laxURIParseOptions' -- allows brackets
      -- in the queries, which draws us closer to the WHATWG URL standard.
      uri' <- URIBS.parseURI URIBS.laxURIParserOptions (encodeUtf8 link)
            & either (throwError . ExternalResourceInvalidUri) pure

      -- We stick to our infrastructure by continuing to operate on the datatypes
      -- from `modern-uri`, which are used in the 'req' library. First we
      -- serialize our URI parsed with 'parseURI' so it becomes a 'ByteString'
      -- with all the necessary special characters *percent-encoded*, and then
      -- call 'mkURIBs'.
      mkURIBs (URIBS.serializeURIRef' uri')
           -- Ideally, this exception should never be thrown, as the URI
           -- already *percent-encoded* with 'parseURI' from 'uri-bytestring'
           -- and 'mkURIBs' is only used to convert to 'URI' type from
           -- 'modern-uri' package.
           & handleJust (fromException @ParseExceptionBs)
           (throwError . ExternalResourceUriConversionError)

checkExternalResource :: Config -> Text -> IO (VerifyResult VerifyError)
checkExternalResource Config{..} link
  | isIgnored = return mempty
  | otherwise = fmap toVerifyRes $ runExceptT $ do
      uri <- parseUri link
      case toString <$> uriScheme uri of
        Just "http" -> checkHttp uri
        Just "https" -> checkHttp uri
        Just "ftp" -> checkFtp uri False
        Just "ftps" -> checkFtp uri True
        _ -> throwError ExternalResourceUnknownProtocol
  where
    ExclusionConfig{..} = cExclusions
    NetworkingConfig{..} = cNetworking

    isIgnored = doesMatchAnyRegex link ecIgnoreExternalRefsTo

    doesMatchAnyRegex :: Text -> ([Regex] -> Bool)
    doesMatchAnyRegex src = any $ \regex ->
      case regexec regex src of
        Right res -> case res of
          Just (before, match, after, _) ->
            null before && null after && not (null match)
          Nothing -> False
        Left _ -> False

    checkHttp :: URI -> ExceptT VerifyError IO ()
    checkHttp uri = makeHttpRequest uri HEAD 0.3 `catchError` \case
      e | isFixable e -> throwError e
      _ -> makeHttpRequest uri GET 0.7

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

      let maxTime = Time @Second $ unTime ncExternalRefCheckTimeout * timeoutFrac

      mres <- liftIO (timeout maxTime $ void reqLink) `catch`
        (either throwError (\() -> return (Just ())) . interpretErrors)
      maybe (throwError $ ExternalResourceSomeError "Response timeout") pure mres

    isAllowedErrorCode = or . sequence
      -- We have to stay conservative - if some URL can be accessed under
      -- some circumstances, we should do our best to report it as fine.
      [ if ncIgnoreAuthFailures -- unauthorized access
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
            | otherwise -> case statusCode (responseStatus resp) of
              429 -> Left . ExternalHttpTooManyRequests $ retryAfterInfo resp
              _ -> Left . ExternalHttpResourceUnavailable $ responseStatus resp
          other -> Left . ExternalResourceSomeError $ show other
      where
        retryAfterInfo :: Response a -> Maybe RetryAfter
        retryAfterInfo = readMaybe . decodeUtf8 <=< L.lookup hRetryAfter . responseHeaders

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
      \ftpHandle response -> do
        -- check connection status
        when (frStatus response /= Success) $
          throwError $ ExternalFtpResourceUnavailable response
        -- anonymous login
        loginResp <- login ftpHandle "anonymous" ""
        -- check login status
        when (frStatus loginResp /= Success) $
          if ncIgnoreAuthFailures
          then pure ()
          else throwError $ ExternalFtpException $ UnsuccessfulException loginResp
        -- If the response is non-null, the path is definitely a directory;
        -- If the response is null, the path may be a file or may not exist.
        dirList <- nlst ftpHandle [ "-a", path ]
        when (BS.null dirList) $ do
          -- The server-PI will respond to the SIZE command with a 213 reply
          -- giving the transfer size of the file whose pathname was supplied,
          -- or an error response if the file does not exist, the size is
          -- unavailable, or some other error has occurred.
          _ <- size ftpHandle path `catch` \case
              UnsuccessfulException _ -> throwError $ FtpEntryDoesNotExist path
              FailureException FTPResponse{..} | frCode == 550 ->
                throwError $ FtpEntryDoesNotExist path
              err -> liftIO $ throwIO err
          pure ()
      where
        handler = if secure then withFTPS else withFTP

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | @loopAsyncUntil ma mb@ will continually run @ma@ until @mb@ throws an exception or returns.
-- Once it does, it'll wait for @ma@ to finish running one last time and then return.
--
-- See #163 to read more on why it's important to let @ma@ finish cleanly.
-- * https://github.com/serokell/xrefcheck/issues/162
-- * https://github.com/serokell/xrefcheck/pull/163
loopAsyncUntil :: forall a b. IO a -> IO b -> IO b
loopAsyncUntil loopingAction action =
  mask $ \restore -> do
    shouldLoop <- newIORef True
    loopingActionAsync <- async $ restore $ loopingAction' shouldLoop
    restore action `finally` do
      writeIORef shouldLoop False
      wait loopingActionAsync
  where
    loopingAction' :: IORef Bool -> IO ()
    loopingAction' shouldLoop = do
      whenM (readIORef shouldLoop) do
        void loopingAction
        loopingAction' shouldLoop
