{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Crv.Verify
    ( -- * General verification
      VerifyResult (..)
    , verifyOk
    , verifyErrors
    , verifying

    , WithReferenceLoc (..)

      -- * Cross-references validation
    , CrvVerifyError (..)
    , verifyRepo
    , checkExternalResource
    ) where

import Control.Concurrent.Async (forConcurrently, withAsync)
import Control.Monad.Except (ExceptT (..), MonadError (..))
import Data.Default (def)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Metrics (damerauLevenshteinNorm)
import Fmt (Buildable (..), blockListF', listF, (+|), (|+))
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), responseStatus)
import Network.HTTP.Req (GET (..), HEAD (..), HttpException (..), NoReqBody (..), ignoreResponse,
                         parseUrl, req, runReq)
import Network.HTTP.Types.Status (Status, statusCode, statusMessage)
import System.Console.Pretty (Style (..), style)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath.Posix (takeDirectory)
import System.FilePath.Posix ((</>))
import Time (RatioNat, Second, Time (..), ms, threadDelay, timeout)

import Crv.Config
import Crv.Core
import Crv.Progress

-----------------------------------------------------------
-- General verification
-----------------------------------------------------------

newtype VerifyResult e = VerifyResult [e]
    deriving (Show, Functor)

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
        "In file " +| style Faint wrlFile |+ "\nbroken " +| wrlReference |+ "\n"
        +| wrlItem |+ "\n\n"

data CrvVerifyError
    = FileDoesNotExist FilePath
    | AnchorDoesNotExist Text [Anchor]
    | ExternalResourceInvalidUri
    | ExternalResourceUnavailable Status
    | ExternalResourceSomeError Text
    deriving (Show)

instance Buildable CrvVerifyError where
    build = \case
        FileDoesNotExist file ->
            "⛀  File does not exist:\n   " +| file |+ "\n"
        AnchorDoesNotExist anchor similar ->
            "⛀  Anchor '" +| anchor |+ "' is not present" +|
            anchorHints similar
        ExternalResourceInvalidUri ->
            "⛂  Bad url (expected 'http' or 'https')\n"
        ExternalResourceUnavailable status ->
            "⛂  Resource unavailable (" +| statusCode status |+ " " +|
            decodeUtf8 @Text (statusMessage status) |+ ")"
        ExternalResourceSomeError err -> "⛂  " +| build err |+ "\n\n"
      where
        anchorHints = \case
            []  -> "\n"
            [h] -> ",\n   did you mean " +| h |+ "?\n"
            hs  -> ", did you mean:\n" +| blockListF' "    -" build hs

verifyRepo
    :: Rewrite
    -> VerifyConfig
    -> FilePath
    -> RepoInfo
    -> IO (VerifyResult $ WithReferenceLoc CrvVerifyError)
verifyRepo rw config@VerifyConfig{..} root repoInfo'@(RepoInfo repoInfo) = do
    progressRef <- newIORef $ initVerifyProgress repoInfo'
    withAsync (printer progressRef) $ \_ ->
        fmap fold . forConcurrently (M.toList repoInfo) $ \(file, fileInfo) ->
            fmap fold . forConcurrently (_fiReferences fileInfo) $ \ref ->
                verifyReference config progressRef repoInfo' root file ref
  where
    printer progressRef = forever $ do
        readIORef progressRef >>= reprintAnalyseProgress rw
        threadDelay (ms 100)

verifyReference
    :: VerifyConfig
    -> IORef VerifyProgress
    -> RepoInfo
    -> FilePath
    -> FilePath
    -> Reference
    -> IO (VerifyResult $ WithReferenceLoc CrvVerifyError)
verifyReference config@VerifyConfig{..} progressRef (RepoInfo repoInfo)
                root containingFile ref@Reference{..} = do
    res <- case locationType rLink of
        LocalLoc    -> checkFileRef rAnchor containingFile
        RelativeLoc -> checkFileRef rAnchor
                      (takeDirectory containingFile </> toString rLink)
        AbsoluteLoc -> checkFileRef rAnchor (root <> toString rLink)
        ExternalLoc -> checkExternalResource config rLink
        OtherLoc    -> verifying pass

    let moveProgress =
            incProgress .
            (if verifyOk res then id else incProgressErrors)

    atomicModifyIORef' progressRef $ \VerifyProgress{..} ->
        ( if isExternal (locationType rLink)
          then VerifyProgress{ vrExternal = moveProgress vrExternal, .. }
          else VerifyProgress{ vrLocal = moveProgress vrLocal, .. }
        , ()
        )
    return $ fmap (WithReferenceLoc containingFile ref) res
  where
    checkFileRef mAnchor file = verifying $ do
        fileExists <- liftIO $ doesFileExist file
        dirExists <- liftIO $ doesDirectoryExist file
        unless (fileExists || dirExists) $
            throwError (FileDoesNotExist file)

        case M.lookup file repoInfo of
            Nothing -> pass  -- no support for such file, can do nothing
            Just referedFileInfo ->
                whenJust mAnchor $ checkAnchorExists (_fiAnchors referedFileInfo)

    checkAnchorExists givenAnchors anchor =
        case find ((== anchor) . aName) givenAnchors of
            Just _ -> pass
            Nothing ->
                let isSimilar = (>= vcAnchorSimilarityThreshold)
                    similarAnchors =
                        filter (isSimilar . realToFrac . damerauLevenshteinNorm anchor . aName)
                        givenAnchors
                in throwError $ AnchorDoesNotExist anchor similarAnchors

checkExternalResource :: VerifyConfig
                      -> Text
                      -> IO (VerifyResult CrvVerifyError)
checkExternalResource VerifyConfig{..} link
    | doesReferLocalhost = return mempty
    | otherwise = fmap toVerifyRes $ do
        makeRequest HEAD 0.3 >>= \case
            Right () -> return $ Right ()
            Left   _ -> makeRequest GET 0.7
  where
    doesReferLocalhost = any (`T.isInfixOf` link) ["://localhost", "://127.0.0.1"]

    makeRequest :: _ => method -> RatioNat -> IO (Either CrvVerifyError ())
    makeRequest method timeoutFrac = runExceptT $ do
        parsedUrl <- parseUrl (encodeUtf8 link)
                   & maybe (throwError ExternalResourceInvalidUri) pure
        let reqLink = case parsedUrl of
                Left (url, option) ->
                    runReq def $ req method url NoReqBody ignoreResponse option
                Right (url, option) ->
                    runReq def $ req method url NoReqBody ignoreResponse option

        let maxTime = Time @Second $ unTime vcExternalRefCheckTimeout * timeoutFrac

        mres <- liftIO (timeout maxTime $ void reqLink)
                `catch` (either throwError (\() -> return (Just ())) . interpretErrors)
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
                    | otherwise -> Left $ ExternalResourceUnavailable (responseStatus resp)
                other -> Left . ExternalResourceSomeError $ show other
