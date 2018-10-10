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

import Control.Monad.Except (ExceptT (..), MonadError (..))
import Data.Default (def)
import qualified Data.Map as M
import Fmt (Buildable (..), listF, (+|), (|+))
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), responseStatus)
import Network.HTTP.Req (GET (..), HEAD (..), HttpException (..), NoReqBody (..), ignoreResponse,
                         parseUrl, req, runReq)
import Network.HTTP.Types.Status (Status, statusCode, statusMessage)
import System.Console.Pretty (Style (Faint), style)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath.Posix (takeDirectory)
import System.FilePath.Posix ((</>))
import System.Timeout (timeout)

import Crv.Core

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
        "In file " +| style Faint wrlFile |+ " " +| wrlReference |+ "\n"
        +| wrlItem |+ "\n\n"

data CrvVerifyError
    = FileDoesNotExist FilePath
    | AnchorDoesNotExist Text [Anchor]
    | ExternalResourceInvalidUri Text
    | ExternalResourceUnavailable Text Status
    | ExternalResourceSomeError Text
    deriving (Show)

instance Buildable CrvVerifyError where
    build = \case
        FileDoesNotExist file ->
            "File does not exist: " +| file |+ ""
        AnchorDoesNotExist anchor available ->
            "Anchor '" +| anchor |+ "' does not exist in the file, available ones: "
            +| listF available |+ ""
        ExternalResourceInvalidUri link ->
            "Bad url: " +| link |+ ", expected 'http' or 'https'"
        ExternalResourceUnavailable link status ->
            "Resource unavailable: (" +| statusCode status |+ " " +|
            decodeUtf8 @Text (statusMessage status) |+ "): " +| link |+ ""
        ExternalResourceSomeError err -> build err

verifyRepo :: FilePath
           -> RepoInfo
           -> IO (VerifyResult $ WithReferenceLoc CrvVerifyError)
verifyRepo root (RepoInfo repoInfo) =
    concatForM (M.toList repoInfo) $ \(file, fileInfo) ->
        concatForM (_fiReferences fileInfo) $ \ref@Reference{..} ->
            fmap (fmap $ WithReferenceLoc file ref) $
            case locationType rLink of
                LocalLoc    -> checkFileRef rAnchor file
                RelativeLoc -> checkFileRef rAnchor
                               (takeDirectory file </> toString rLink)
                AbsoluteLoc -> checkFileRef rAnchor (root <> toString rLink)
                ExternalLoc -> return mempty  -- checkExternalResource rLink
  where
    checkFileRef mAnchor file = verifying $ do
        fileExists <- liftIO $ doesFileExist file
        dirExists <- liftIO $ doesDirectoryExist file
        unless (fileExists || dirExists) $
            throwError (FileDoesNotExist file)

        case M.lookup file repoInfo of
            Nothing -> pass  -- no support for such file, can do nothing
            Just referedFileInfo ->
                whenJust mAnchor $ \anchor -> do
                    let fileAnchors = _fiAnchors referedFileInfo
                    maybe (throwError $ AnchorDoesNotExist anchor fileAnchors) pure $
                        void $ find ((== anchor) . aName) fileAnchors

checkExternalResource :: Text -> IO (VerifyResult CrvVerifyError)
checkExternalResource link = fmap toVerifyRes $ do
    makeRequest HEAD >>= \case
        Right () -> return $ Right ()
        Left (ExternalResourceUnavailable _ status) | statusCode status == 405
                                                   || statusCode status == 418
                 -> makeRequest GET
        Left err -> pure (Left err)
  where
    makeRequest :: _ => method -> IO (Either CrvVerifyError ())
    makeRequest method = runExceptT $ do
        parsedUrl <- parseUrl (encodeUtf8 link)
                  & maybe (throwError $ ExternalResourceInvalidUri link) pure
        let reqLink = case parsedUrl of
                Left (url, option) ->
                    runReq def $ req method url NoReqBody ignoreResponse option
                Right (url, option) ->
                    runReq def $ req method url NoReqBody ignoreResponse option

        -- TODO: tunable timeout
        mres <- liftIO (timeout 3000000 $ void reqLink) `catch` (throwError . processErrors)
        maybe (throwError $ ExternalResourceSomeError "Response timeout") pure mres

    processErrors = \case
        JsonHttpException _ -> error "External link JSON parse exception"
        VanillaHttpException err -> case err of
            InvalidUrlException{} -> error "External link URL invalid exception"
            HttpExceptionRequest _ exc -> case exc of
                StatusCodeException resp _ ->
                    ExternalResourceUnavailable link (responseStatus resp)
                other -> ExternalResourceSomeError $ show other
