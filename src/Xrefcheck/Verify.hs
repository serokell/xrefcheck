{- SPDX-FileCopyrightText: 2018-2021 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Xrefcheck.Verify
  ( -- * General verification
    VerifyResult (..)
  , verifyErrors
  , verifying

  , RetryAfter (..)
  , WithReferenceLoc (..)

    -- * Concurrent traversal with caching
  , NeedsCaching (..)
  , forConcurrentlyCaching

    -- * Cross-references validation
  , DomainName (..)
  , VerifyError (..)
  , verifyRepo
  , verifyReference
  , checkExternalResource
  , reportVerifyErrs
  ) where

import Universum

import Control.Concurrent.Async (Async, async, cancel, poll, wait, withAsync)
import Control.Exception (AsyncException (..), throwIO)
import Control.Exception.Safe (handleAsync)
import Control.Monad.Except (MonadError (..))
import Data.Bits (toIntegralSized)
import Data.ByteString qualified as BS
import Data.List (lookup)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Reflection (Given)
import Data.Set qualified as S
import Data.Text (toCaseFold)
import Data.Text.Metrics (damerauLevenshteinNorm)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, readPTime, rfc822DateFormat)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Traversable (for)
import Fmt (Buildable (..), Builder, fmt, maybeF, nameF)
import GHC.Exts qualified as Exts
import GHC.Read (Read (readPrec))
import Network.FTP.Client
  (FTPException (..), FTPResponse (..), ResponseStatus (..), login, nlst, size, withFTP, withFTPS)
import Network.HTTP.Client
  (HttpException (..), HttpExceptionContent (..), Response, responseHeaders, responseStatus)
import Network.HTTP.Req
  (AllowsBody, CanHaveBody (NoBody), GET (..), HEAD (..), HttpBodyAllowed,
  HttpConfig (httpConfigRedirectCount), HttpException (..), HttpMethod, NoReqBody (..),
  defaultHttpConfig, ignoreResponse, req, runReq, useURI)
import Network.HTTP.Types.Header (hRetryAfter)
import Network.HTTP.Types.Status (Status, statusCode, statusMessage)
import Text.Interpolation.Nyan
import Text.ParserCombinators.ReadPrec qualified as ReadPrec (lift)
import Text.URI (Authority (..), URI (..), relativeTo, render, unRText)
import Time (RatioNat, Second, Time (..), ms, sec, threadDelay, timeout, (+:+), (-:-))

import Control.Monad.Trans.Except (withExceptT)
import Xrefcheck.Config
import Xrefcheck.Core
import Xrefcheck.Data.URI
import Xrefcheck.Orphans ()
import Xrefcheck.Progress
import Xrefcheck.Scan
import Xrefcheck.Scanners.Markdown (MarkdownConfig (mcFlavor))
import Xrefcheck.System
import Xrefcheck.Util

{-# ANN module ("HLint: ignore Use uncurry" :: Text) #-}
{-# ANN module ("HLint: ignore Use 'runExceptT' from Universum" :: Text) #-}

-----------------------------------------------------------
-- General verification
-----------------------------------------------------------

newtype VerifyResult e = VerifyResult [e]
  deriving newtype (Show, Eq, Functor)

deriving newtype instance Semigroup (VerifyResult e)
deriving newtype instance Monoid (VerifyResult e)

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
  { wrlFile      :: RelPosixLink
  , wrlReference :: Reference
  , wrlItem      :: a
  }

instance (Given ColorMode, Buildable a) => Buildable (WithReferenceLoc a) where
  build WithReferenceLoc{..} = [int||
    In file #{styleIfNeeded Faint (styleIfNeeded Bold wrlFile)}
    bad #{wrlReference}
    #{wrlItem}
    |]

-- | Contains a name of a domain, examples:
-- @DomainName "github.com"@,
-- @DomainName "localhost"@,
-- @DomainName "192.168.0.104"@
newtype DomainName = DomainName { unDomainName :: Text }
  deriving stock (Show, Eq, Ord)

data VerifyError
  = LocalFileDoesNotExist RelPosixLink
  | LocalFileOutsideRepo RelPosixLink
  | LinkTargetNotAddedToGit RelPosixLink
  | AnchorDoesNotExist Text [Anchor]
  | AmbiguousAnchorRef RelPosixLink Text (NonEmpty Anchor)
  | ExternalResourceUriParseError UriParseError
  | ExternalResourceInvalidUrl (Maybe Text)
  | ExternalResourceUnknownProtocol
  | ExternalHttpResourceUnavailable Status
  | ExternalHttpTooManyRequests (Maybe RetryAfter) (Maybe DomainName)
  | ExternalHttpTimeout (Maybe DomainName)
  | ExternalFtpResourceUnavailable FTPResponse
  | ExternalFtpException FTPException
  | FtpEntryDoesNotExist FilePath
  | ExternalResourceSomeError Text
  | RedirectChainCycle RedirectChain
  | RedirectMissingLocation RedirectChain
  | RedirectChainLimit RedirectChain
  | RedirectRuleError RedirectChain (Maybe RedirectRuleOn)
  deriving stock (Show, Eq)

data ResponseResult
  = RRDone
  | RRFollow Text

instance Given ColorMode => Buildable VerifyError where
  build = \case
    LocalFileDoesNotExist file ->
      [int||
      File does not exist:
        #{file}#l{
          if hasBackslash file
          then "\\n  Its reference contains a backslash. Maybe it uses the wrong path separator."
          else ""
        }
      |]

    LocalFileOutsideRepo file ->
      [int||
      Link targets a local file outside repository:
        #{file}
      |]

    LinkTargetNotAddedToGit file ->
      [int||
      Link target is not tracked by Git:
        #{file}
        Please run "git add" before running xrefcheck or enable --include-untracked CLI option.
      |]

    AnchorDoesNotExist anchor similar -> case nonEmpty similar of
      Nothing ->
        [int||
        Anchor '#{anchor}' is not present
        |]
      Just otherAnchors ->
        [int||
        Anchor '#{anchor}' is not present, did you mean:
        #{interpolateIndentF 4 $ interpolateBlockListF otherAnchors}
        |]

    AmbiguousAnchorRef file anchor fileAnchors ->
      [int||
      Ambiguous reference to anchor '#{anchor}'
        In file #{file}
        It could refer to either:
      #{interpolateIndentF 4 $ interpolateBlockListF fileAnchors}
        Use of ambiguous anchors is discouraged because the target
        can change silently while the document containing it evolves.
      |]

    ExternalResourceUriParseError (UPEInvalid err) ->
      [int||
      Invalid URI (#{err})
      |]

    ExternalResourceUriParseError (UPEConversion err) ->
      [int||
      Invalid URI
      #{interpolateIndentF 4 . build $ displayException err}
      |]

    ExternalResourceInvalidUrl Nothing ->
      [int||
      Invalid URL
      |]

    ExternalResourceInvalidUrl (Just message) ->
      [int||
      Invalid URL (#{message})
      |]

    ExternalResourceUnknownProtocol ->
      [int||
      Bad url (expected 'http','https', 'ftp' or 'ftps')
      |]

    ExternalHttpResourceUnavailable status ->
      [int||
      Resource unavailable (#{statusCode status} #{decodeUtf8 @Text (statusMessage status)})
      |]

    ExternalHttpTooManyRequests retryAfter _ ->
      [int||
      Resource unavailable (429 Too Many Requests; retry after #{maybeF retryAfter})
      |]

    ExternalHttpTimeout _ ->
      [int||
      Response timeout
      |]

    ExternalFtpResourceUnavailable response ->
      [int||
      Resource unavailable:
      #{response}
      |]

    ExternalFtpException err ->
      [int||
      FTP exception (#{err})
      |]

    FtpEntryDoesNotExist entry ->
      [int||
      File or directory does not exist:
      #{entry}
      |]

    ExternalResourceSomeError err ->
      [int||
      #{err}
      |]

    RedirectChainCycle chain ->
      [int||
      Cycle found in the following redirect chain:
      #{interpolateIndentF 2 $ attachToRedirectChain chain "here"}
      |]

    RedirectMissingLocation chain ->
      [int||
      Missing location header in the following redirect chain:
      #{interpolateIndentF 2 $ attachToRedirectChain chain "no location header"}
      |]

    RedirectChainLimit chain ->
        [int||
        The follow redirects limit has been reached in the following redirect chain:
        #{interpolateIndentF 2 $ attachToRedirectChain chain "stopped before this one"}
        |]

    RedirectRuleError chain mOn ->
      [int||
      #{redirect} found:
      #{interpolateIndentF 2 $ attachToRedirectChain chain "stopped before this one"}
      |]
      where
        redirect :: Text
        redirect = case mOn of
          Nothing -> "Redirect"
          Just RROPermanent -> "Permanent redirect"
          Just RROTemporary -> "Temporary redirect"
          Just (RROCode code) -> show code <> " redirect"

attachToRedirectChain :: RedirectChain -> Text -> Builder
attachToRedirectChain chain attached
  = build chain <> build attachedText
  where
    attachedText = "\n   ^-- " <> attached

data RetryCounter = RetryCounter
  { rcTotalRetries   :: Int
  , rcTimeoutRetries :: Int
  } deriving stock (Show)

incTotalCounter :: RetryCounter -> RetryCounter
incTotalCounter rc = rc {rcTotalRetries = rcTotalRetries rc + 1}

incTimeoutCounter :: RetryCounter -> RetryCounter
incTimeoutCounter rc = rc {rcTimeoutRetries = rcTimeoutRetries rc + 1}

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
  -> RepoInfo
  -> IO (VerifyResult $ WithReferenceLoc VerifyError)
verifyRepo
  rw
  config@Config{..}
  mode
  repoInfo@RepoInfo{..}
    = do
  let toScan = do
        (canonicalFile, (file, fileInfo)) <- toPairs riFiles
        guard . not $ matchesGlobPatterns (ecIgnoreRefsFrom cExclusions) canonicalFile
        case fileInfo of
          Scanned fi -> do
            ref <- _fiReferences fi
            return (file, ref)
          NotScannable -> empty -- No support for such file, can do nothing.
          NotAddedToGit -> empty -- If this file is scannable, we've notified
                                 -- user that we are scanning only files
                                 -- added to Git while gathering RepoInfo.

  progressRef <- newIORef $ initVerifyProgress (map snd toScan)
  domainsReturned429Ref <- newIORef S.empty
  accumulated <- loopAsyncUntil (printer progressRef) do
    forConcurrentlyCaching toScan ifExternalThenCache $ \(file, ref) ->
      verifyReference config mode domainsReturned429Ref progressRef repoInfo file ref
  case accumulated of
    Right res -> return $ fold res
    Left (exception, partialRes) -> do
      -- The user has hit Ctrl+C; display any verification errors we managed to find and exit.
      let errs = verifyErrors (fold partialRes)
          total = length toScan
          checked = length partialRes
      whenJust errs reportVerifyErrs
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
    ifExternalThenCache (_, Reference{..}) =
      case rInfo of
        RIExternal (ELUrl url) ->
          CacheUnderKey url
        _ ->
          NoCaching

shouldCheckLocType :: VerifyMode -> ReferenceInfo -> Bool
shouldCheckLocType mode rInfo =
  case rInfo of
    RIFile _ -> shouldCheckLocal mode
    RIExternal (ELUrl _) -> shouldCheckExternal mode
    RIExternal (ELOther _) -> False

verifyReference
  :: Config
  -> VerifyMode
  -> IORef (S.Set DomainName)
  -> IORef VerifyProgress
  -> RepoInfo
  -> RelPosixLink
  -> Reference
  -> IO (VerifyResult $ WithReferenceLoc VerifyError)
verifyReference
  config@Config{..}
  mode
  domainsReturned429Ref
  progressRef
  repoInfo
  file
  ref@Reference{..}
    = fmap (fmap addReference . toVerifyRes) $
      retryVerification (RetryCounter 0 0) $ runExceptT $
        if shouldCheckLocType mode rInfo
        then case rInfo of
          RIFile ReferenceInfoFile{..} ->
            case rifLink of
              FLLocal ->
                checkRef rifAnchor file
              FLRelative link ->
                checkRef rifAnchor $ takeDirectory file </> link
              FLAbsolute link ->
                checkRef rifAnchor link
          RIExternal (ELUrl url) ->
            checkExternalResource emptyChain config url
          RIExternal (ELOther _) ->
            pass
        else pass
  where
    addReference :: VerifyError -> WithReferenceLoc VerifyError
    addReference = WithReferenceLoc file ref

    retryVerification
      :: RetryCounter
      -> IO (Either VerifyError ())
      -> IO (Either VerifyError ())
    retryVerification rc resIO = do
      res <- resIO
      case res of
        -- Success
        Right () -> modifyProgressRef Nothing reportSuccess $> res
        Left err -> do
          setOfReturned429 <- addDomainIf429 domainsReturned429Ref err
          case decideWhetherToRetry setOfReturned429 rc err of
            -- Unfixable
            Nothing -> modifyProgressRef Nothing reportError $> res
            -- Fixable, retry
            Just (mbCurrentRetryAfter, counterModifier) -> do
              now <- getPOSIXTime <&> posixTimeToTimeSecond

              let toSeconds = \case
                    Seconds s -> s
                    -- Calculates the seconds left until @Retry-After@ date.
                    -- Defaults to 0 if the date has already passed.
                    Date date | utcTimeToTimeSecond date >= now -> utcTimeToTimeSecond date -:- now
                    _ -> sec 0

              let currentRetryAfter = fromMaybe (ncDefaultRetryAfter cNetworking) $
                    fmap toSeconds mbCurrentRetryAfter

              modifyProgressRef (Just (now, currentRetryAfter)) reportRetry
              threadDelay currentRetryAfter
              retryVerification (counterModifier rc) resIO

    modifyProgressRef
      :: Maybe (Time Second, Time Second)
      -> (forall w. Ord w => w -> Progress Int w -> Progress Int w)
      -> IO ()
    modifyProgressRef mbRetryData moveProgress = atomicModifyIORef' progressRef $ \VerifyProgress{..} ->
      ( case rInfo of
          RIFile _ -> VerifyProgress{ vrLocal = moveProgress () vrLocal, .. }
          RIExternal (ELOther _) -> VerifyProgress{ vrLocal = moveProgress () vrLocal, .. }
          RIExternal (ELUrl url) -> VerifyProgress{ vrExternal =
            let vrExternalAdvanced = moveProgress url vrExternal
            in  case mbRetryData of
                  Just (now, retryAfter) -> case getTaskTimestamp vrExternal of
                    Just (TaskTimestamp ttc start)
                      | retryAfter +:+ now <= ttc +:+ start -> vrExternalAdvanced
                    _ -> setTaskTimestamp url retryAfter now vrExternalAdvanced
                  Nothing -> vrExternalAdvanced, .. }
      , ()
      )

    addDomainIf429 :: IORef (S.Set DomainName) -> VerifyError -> IO (S.Set DomainName)
    addDomainIf429 setRef err = atomicModifyIORef' setRef $ \s ->
      (\x -> (x, x)) $ case err of
      ExternalHttpTooManyRequests _ mbDomain ->
        maybe s (flip S.insert s) mbDomain
      _ -> s

    decideWhetherToRetry
      :: S.Set DomainName
      -> RetryCounter
      -> VerifyError
      -> Maybe (Maybe RetryAfter, RetryCounter -> RetryCounter)
    decideWhetherToRetry setOfReturned429 rc = \case
      ExternalHttpTooManyRequests retryAfter _
        | totalRetriesNotExceeded -> Just (retryAfter, incTotalCounter)
      ExternalHttpTimeout (Just domain)
        | totalRetriesNotExceeded && timeoutRetriesNotExceeded ->
          -- If a given domain ever returned 429 error, we assume that getting timeout from
          -- the domain can be considered as a 429-like error, and hence we retry.
          -- If there was no 429 responses from this domain, then getting timeout from
          -- it probably means that this site is not working at all.
          -- Also, there always remains a possibility that we just didn't get the response
          -- in time, but we can't avoid this case here, the only thing that can help
          -- is to increase the allowed timeout in the config.

          if S.member domain setOfReturned429
          then Just (Just (Seconds $ sec 0), incTimeoutCounter . incTotalCounter)
          else Nothing
      _ -> Nothing
      where
        totalRetriesNotExceeded = rcTotalRetries rc < ncMaxRetries cNetworking
        timeoutRetriesNotExceeded = rcTimeoutRetries rc < ncMaxTimeoutRetries cNetworking

    isVirtual = matchesGlobPatterns (ecIgnoreLocalRefsTo cExclusions)

    -- Checks a local file reference.
    checkRef :: Maybe Text -> RelPosixLink -> ExceptT VerifyError IO ()
    checkRef mAnchor referredFile = do
      let canonicalFile = canonicalizeRelPosixLink referredFile
      unless (isVirtual canonicalFile) do
        when (hasUnexpanededParentIndirections canonicalFile) $
          throwError $ LocalFileOutsideRepo referredFile

        mFileStatus <- tryGetFileStatus referredFile
        case mFileStatus of
          Right (Scanned referredFileInfo) -> whenJust mAnchor $
            checkAnchor referredFile (_fiAnchors referredFileInfo)
          Right NotAddedToGit -> throwError (LinkTargetNotAddedToGit referredFile)
          Left UntrackedDirectory -> throwError (LinkTargetNotAddedToGit referredFile)
          Right NotScannable -> pass -- no support for such file, can do nothing
          Left TrackedDirectory -> pass -- path leads to directory, currently
                                        -- if such link contain anchor, we ignore it

    caseInsensitive = caseInsensitiveAnchors . mcFlavor . scMarkdown $ cScanners

    -- Returns `Nothing` when path corresponds to an existing (and tracked) directory
    tryGetFileStatus :: RelPosixLink -> ExceptT VerifyError IO (Either DirectoryStatus FileStatus)
    tryGetFileStatus filePath
      | Just f <- lookupFile canonicalFile repoInfo = return (Right f)
      | Just d <- lookupDirectory canonicalFile repoInfo = return (Left d)
      | otherwise = throwError (LocalFileDoesNotExist filePath)
      where
        canonicalFile = canonicalizeRelPosixLink filePath

    checkAnchor filePath fileAnchors anchor = do
      checkAnchorReferenceAmbiguity filePath fileAnchors anchor
      checkDeduplicatedAnchorReference filePath fileAnchors anchor
      checkAnchorExists fileAnchors anchor

    anchorNameEq =
      if caseInsensitive
      then (==) `on` toCaseFold
      else (==)

    -- Detect a case when original file contains two identical anchors, github
    -- has added a suffix to the duplicate, and now the original is referrenced -
    -- such links are pretty fragile and we discourage their use despite
    -- they are in fact unambiguous.
    checkAnchorReferenceAmbiguity filePath fileAnchors anchor = do
      let similarAnchors = filter (anchorNameEq anchor . aName) fileAnchors
      when (length similarAnchors > 1) $
        throwError $ AmbiguousAnchorRef filePath anchor (Exts.fromList similarAnchors)

    -- Similar to the previous one, but for the case when we reference the
    -- renamed duplicate.
    checkDeduplicatedAnchorReference filePath fileAnchors anchor =
      whenJust (stripAnchorDupNo anchor) $ \origAnchor ->
        checkAnchorReferenceAmbiguity filePath fileAnchors origAnchor

    checkAnchorExists givenAnchors anchor =
      case find (anchorNameEq anchor . aName) givenAnchors of
        Just _ -> pass
        Nothing ->
          let isSimilar = (>= scAnchorSimilarityThreshold cScanners)
              distance = damerauLevenshteinNorm `on` toCaseFold
              similarAnchors = flip filter givenAnchors
                $ isSimilar
                . realToFrac
                . distance anchor
                . aName
          in throwError $ AnchorDoesNotExist anchor similarAnchors

checkExternalResource :: RedirectChain -> Config -> Text -> ExceptT VerifyError IO ()
checkExternalResource followed config@Config{..} link
  | isIgnored = pass
  | followed `hasRequest` (RedirectChainLink link) =
      throwError $ RedirectChainCycle $ followed `pushRequest` (RedirectChainLink link)
  | ncMaxRedirectFollows >= 0 && totalFollowed followed > ncMaxRedirectFollows =
      throwError $ RedirectChainLimit $ followed `pushRequest` (RedirectChainLink link)
  | otherwise = do
      uri <- ExternalResourceUriParseError `withExceptT` parseUri False link
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

    checkHttp :: URI -> ExceptT VerifyError IO ()
    checkHttp uri = makeHttpRequest uri HEAD 0.3 `catchError` \case
      e@(ExternalHttpTooManyRequests _ _) -> throwError e
      _ -> makeHttpRequest uri GET 0.7

    httpConfig :: HttpConfig
    httpConfig = defaultHttpConfig { httpConfigRedirectCount = 0 }

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
              runReq httpConfig $
                req method url NoReqBody ignoreResponse option
            Right (url, option) ->
              runReq httpConfig $
                req method url NoReqBody ignoreResponse option

      let maxTime = Time @Second $ unTime ncExternalRefCheckTimeout * timeoutFrac

      reqRes <- catch (liftIO (timeout maxTime $ reqLink $> RRDone)) $
         (Just <$>) <$> interpretErrors uri

      case reqRes of
        Nothing -> throwError $ ExternalHttpTimeout $ extractHost uri
        Just RRDone -> pass
        Just (RRFollow nextLink) ->
          checkExternalResource (followed `pushRequest` (RedirectChainLink link)) config nextLink

    extractHost :: URI -> Maybe DomainName
    extractHost =
      either (const Nothing) (Just . DomainName . unRText . authHost) . uriAuthority

    isAllowedErrorCode :: Int -> Bool
    isAllowedErrorCode = or . sequence
      -- We have to stay conservative - if some URL can be accessed under
      -- some circumstances, we should do our best to report it as fine.
      [ if ncIgnoreAuthFailures -- unauthorized access
        then flip elem [403, 401]
        else const False
      , (405 ==) -- method mismatch
      ]

    interpretErrors uri = \case
      JsonHttpException _ -> error "External link JSON parse exception"
      VanillaHttpException err -> case err of
        InvalidUrlException{} -> error "External link URL invalid exception"
        HttpExceptionRequest _ exc -> case exc of
          StatusCodeException resp _
            | isRedirectCode code -> case redirectLocation of
                Nothing -> throwError $ RedirectMissingLocation $ followed `pushRequest` RedirectChainLink link
                Just nextLink -> do
                  nextUri <- ExternalResourceUriParseError `withExceptT` parseUri True nextLink
                  nextLinkAbsolute <- case relativeTo nextUri uri of
                    -- This should not happen because uri has been parsed with `parseUri False`
                    Nothing -> error "Not an absolute URL exception"
                    Just absoluteTarget -> pure $ render absoluteTarget
                  case redirectRule link nextLinkAbsolute code ncExternalRefRedirects of
                    Nothing -> pure RRDone
                    Just RedirectRule{..} ->
                      case rrOutcome of
                        RROValid -> pure RRDone
                        RROInvalid -> throwError $ RedirectRuleError
                          (followed `pushRequest` RedirectChainLink link `pushRequest` RedirectChainLink nextLinkAbsolute)
                          rrOn
                        RROFollow -> pure $ RRFollow nextLinkAbsolute
            | isAllowedErrorCode code -> pure RRDone
            | otherwise -> case statusCode (responseStatus resp) of
                429 -> throwError $ ExternalHttpTooManyRequests (retryAfterInfo resp) (extractHost uri)
                _ -> throwError $ ExternalHttpResourceUnavailable $ responseStatus resp
            where
              code :: Int
              code = statusCode $ responseStatus resp

              redirectLocation :: Maybe Text
              redirectLocation = fmap decodeUtf8
                . lookup "Location"
                $ responseHeaders resp
          other -> throwError $ ExternalResourceSomeError $ show other
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
