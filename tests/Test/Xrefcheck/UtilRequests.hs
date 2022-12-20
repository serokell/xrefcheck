{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Test.Xrefcheck.UtilRequests
  ( checkLinkAndProgressWithServer
  , verifyLink
  , verifyReferenceWithProgress
  ) where

import Universum

import Control.Exception qualified as E
import Text.Interpolation.Nyan

import Control.Concurrent (forkIO, killThread)
import Test.Tasty.HUnit (assertBool)
import Xrefcheck.Config
import Xrefcheck.Core
import Xrefcheck.Progress
import Xrefcheck.Scan
import Xrefcheck.System (canonicalizePath)
import Xrefcheck.Util
import Xrefcheck.Verify

checkLinkAndProgressWithServer
  :: IO ()
  -> Text
  -> Progress Int
  -> VerifyResult VerifyError
  -> IO ()
checkLinkAndProgressWithServer mock link progress vrExpectation =
      E.bracket (forkIO mock) killThread $ \_ -> do
        (result, progRes) <- verifyLink link
        flip assertBool (result == vrExpectation) $
          [int||
          Verification results differ: expected
          #{interpolateIndentF 2 (show vrExpectation)}
          but got
          #{interpolateIndentF 2 (show result)}
          |]
        flip assertBool (progRes `progEquiv` progress) $
          [int||
          Expected the progress bar state to be
          #{interpolateIndentF 2 (show progress)}
          but got
          #{interpolateIndentF 2 (show progRes)}
          |]
  where
    -- Check whether the two @Progress@ values are equal up to similarity of their essential
    -- components, ignoring the comparison of @pTaskTimestamp@s, which is done to prevent test
    -- failures when comparing the resulting progress, gotten from running the link
    -- verification algorithm, with the expected one, where @pTaskTimestamp@ is hardcoded
    -- as @Nothing@.
    progEquiv :: Eq a => Progress a -> Progress a -> Bool
    progEquiv p1 p2 = and [ ((==) `on` pCurrent) p1 p2
                          , ((==) `on` pTotal) p1 p2
                          , ((==) `on` pErrorsUnfixable) p1 p2
                          , ((==) `on` pErrorsFixable) p1 p2
                          ]

verifyLink :: Text -> IO (VerifyResult VerifyError, Progress Int)
verifyLink link = do
  let reference = Reference "" link Nothing (Position Nothing) RIExternal
  progRef <- newIORef $ initVerifyProgress [reference]
  result <- verifyReferenceWithProgress reference progRef
  p <- readIORef progRef
  return (result, vrExternal p)

verifyReferenceWithProgress :: Reference -> IORef VerifyProgress -> IO (VerifyResult VerifyError)
verifyReferenceWithProgress reference progRef = do
  canonicalRoot <- canonicalizePath "."
  file <- canonicalizePath ""
  fmap wrlItem <$> verifyReference
    (defConfig GitHub & cExclusionsL . ecIgnoreExternalRefsToL .~ []) FullMode
    progRef (RepoInfo mempty mempty canonicalRoot) file reference
