{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

-- | Printing progress bars.
module Xrefcheck.Progress
  ( -- * Task timestamp
    TaskTimestamp (..)

    -- * Progress
  , Progress (..)
  , initProgress
  , incProgress
  , incProgressUnfixableErrors
  , incProgressFixableErrors
  , decProgressFixableErrors
  , fixableToUnfixable
  , setTaskTimestamp
  , removeTaskTimestamp
  , checkTaskTimestamp
  , showProgress

    -- * Printing
  , Rewrite
  , allowRewrite
  , putTextRewrite
  ) where

import Universum

import Data.Ratio ((%))
import System.Console.Pretty (Color (..), Style (..), color, style)
import Time (Second, Time, sec, unTime, (-:-))

-----------------------------------------------------------
-- Task timestamp
-----------------------------------------------------------

-- | Data type defining a point in time when an anonymous task had started
-- and its time to completion.
data TaskTimestamp = TaskTimestamp
  { ttTimeToCompletion :: Time Second
    -- ^ The amount of time required for the task to be completed.
  , ttStart            :: Time Second
    -- ^ The timestamp of when the task had started, represented by the number of seconds
    -- since the Unix epoch.
  } deriving stock (Show)

-----------------------------------------------------------
-- Progress
-----------------------------------------------------------

-- | Processing progress of any thing.
data Progress a = Progress
  { pTotal         :: a
    -- ^ Overall amount of work.
  , pCurrent           :: a
    -- ^ How much has been completed.
  , pErrorsUnfixable :: !a
    -- ^ How much of the completed work finished with an unfixable error.
  , pErrorsFixable   :: !a
    -- ^ How much of the completed work finished with an error that can be
    -- eliminated upon further verification.
  , pTaskTimestamp   :: Maybe TaskTimestamp
    -- ^ A timestamp of an anonymous timer task, where its time to completion is
    -- the time needed to pass for the action to be retried immediately after.
  } deriving stock (Show)

-- | Initialise null progress.
initProgress :: Num a => a -> Progress a
initProgress a = Progress{ pTotal = a
                         , pCurrent = 0
                         , pErrorsUnfixable = 0
                         , pErrorsFixable = 0
                         , pTaskTimestamp = Nothing
                         }

-- | Increase progress amount.
incProgress :: (Num a) => Progress a -> Progress a
incProgress Progress{..} = Progress{ pCurrent = pCurrent + 1, .. }

-- | Increase the number of unfixable errors.
incProgressUnfixableErrors :: (Num a) => Progress a -> Progress a
incProgressUnfixableErrors Progress{..} = Progress{ pErrorsUnfixable = pErrorsUnfixable + 1
                                                  , ..
                                                  }

-- | Increase the number of fixable errors.
incProgressFixableErrors :: (Num a) => Progress a -> Progress a
incProgressFixableErrors Progress{..} = Progress{ pErrorsFixable = pErrorsFixable + 1
                                                , ..
                                                }

-- | Decrease the number of fixable errors. This function indicates the situation where one of
-- such errors had been successfully eliminated.
decProgressFixableErrors :: (Num a) => Progress a -> Progress a
decProgressFixableErrors Progress{..} = Progress{ pErrorsFixable = pErrorsFixable - 1
                                                , ..
                                                }

fixableToUnfixable :: (Num a) => Progress a -> Progress a
fixableToUnfixable Progress{..} = Progress{ pErrorsFixable = pErrorsFixable - 1
                                          , pErrorsUnfixable = pErrorsUnfixable + 1
                                          , ..
                                          }

setTaskTimestamp :: Time Second -> Time Second -> Progress a -> Progress a
setTaskTimestamp ttc startTime Progress{..} = Progress{ pTaskTimestamp =
                                                          Just $ TaskTimestamp ttc startTime
                                                      , ..
                                                      }

removeTaskTimestamp :: Progress a -> Progress a
removeTaskTimestamp Progress{..} = Progress{ pTaskTimestamp = Nothing
                                           , ..
                                           }

checkTaskTimestamp :: Time Second -> Progress a -> Progress a
checkTaskTimestamp posixTime p@Progress{..} =
  case pTaskTimestamp of
    Nothing -> p
    Just TaskTimestamp{..} ->
      if ttTimeToCompletion >= posixTime -:- ttStart
      then p
      else removeTaskTimestamp p

-- | Visualise progress bar.
showProgress :: Text -> Int -> Color -> Time Second -> Progress Int -> Text
showProgress name width col posixTime Progress{..} = mconcat
  [ color col (name <> ": [")
  , toText bar
  , timer
  , color col "]"
  , status
  ]
  where
    -- | Each of the following values represents the number of the progress bar cells
    -- corresponding to the respective "class" of processed references: the valid ones,
    -- the ones containing an unfixable error (a.k.a. the invalid ones), and the ones
    -- containing a fixable error.
    --
    -- The current overall number of proccessed errors.
    done = floor $ (pCurrent % pTotal) * fromIntegral @Int @(Ratio Int) width

    -- | The current number of the invalid references.
    errsU = ceiling $ (pErrorsUnfixable % pTotal) * fromIntegral @Int @(Ratio Int) width

    -- | The current number of (fixable) errors that may be eliminated during further
    -- verification.
    -- Notice!
    --   1. Both this and the previous values use @ceiling@ as the rounding function.
    --      This is done to ensure that as soon as at least 1 faulty reference occurs during
    --      the verification, the cell of its respective color is mathematically guaranteed
    --      to be visible in the progress bar visualization.
    --   2. @errsF@ is bounded from above by @width - errsU@ to prevent an overflow in the
    --      number of the progress bar cells that could be caused by the two @ceilings@s.
    errsF = min (width - errsU) . ceiling $ (pErrorsFixable % pTotal) *
      fromIntegral @Int @(Ratio Int) width

    -- | The number of valid references.
    -- The value is bounded from below by 0 to ensure the number never gets negative.
    -- This situation is plausible due to the different rounding functions used for each value:
    -- @floor@ for the minuend @done@, @ceiling@ for the two subtrahends @errsU@ & @errsF@.
    successful = max 0 $ done - errsU - errsF

    -- | The remaining number of references to be verified.
    remaining = width - successful - errsU - errsF

    bar
      | pTotal == 0 = replicate width '-'
      | otherwise = mconcat
          [ color Blue $ replicate errsF '■'
          , color Red $ replicate errsU '■'
          , color col $ replicate successful '■'
          , color col $ replicate remaining ' '
          , " "
          ]
    timer = case pTaskTimestamp of
      Nothing -> ""
      Just TaskTimestamp{..} -> mconcat
        [ color col "|"
        , color Blue . show . timeSecondCeiling
        $ ttTimeToCompletion -:- (posixTime -:- ttStart)
        ]
    status = mconcat
      [ if pCurrent == pTotal && pErrorsFixable == 0 && pErrorsUnfixable == 0
        then style Faint $ color White "✓"
        else ""
      , if pErrorsFixable /= 0 then color Blue "!" else ""
      , if pErrorsUnfixable /= 0 then color Red "!" else ""
      ]

    timeSecondCeiling :: Time Second -> Time Second
    timeSecondCeiling = sec . fromInteger . ceiling . unTime

-----------------------------------------------------------
-- Rewritable output
-----------------------------------------------------------

-- | Rewrites state.
data RewriteCtx = RewriteCtx
  { rMaxPrintedSize :: IORef Int
  }

-- | Passing this object allows returning caret and replace text in line.
-- Only functions which has this thing can do that because being
-- interleaved with 'putTextLn' printing caret symbol produced garbage.
data Rewrite
  = Rewrite RewriteCtx
    -- ^ Default value.
  | RewriteDisabled
    -- ^ Do not print anything which will be rewritten.
    -- Useful when terminal does not interpret caret returns as expected.

-- | Provide context for rewrite operations.
allowRewrite :: (MonadIO m, MonadMask m) => Bool -> (Rewrite -> m a) -> m a
allowRewrite enabled = bracket prepare erase
  where
    prepare
      | enabled = do
          rMaxPrintedSize <- newIORef 0
          return $ Rewrite RewriteCtx{..}
      | otherwise = pure RewriteDisabled
    erase (Rewrite RewriteCtx{..}) = liftIO $ do
      maxPrintedSize <- readIORef rMaxPrintedSize
      hPutStr stderr $ '\r' : replicate maxPrintedSize ' ' ++ "\r"
    erase RewriteDisabled = pass

-- | Return caret and print the given text.
putTextRewrite :: MonadIO m => Rewrite -> Text -> m ()
putTextRewrite RewriteDisabled _ = pass
putTextRewrite (Rewrite RewriteCtx{..}) msg = do
  liftIO $ hPutStr stderr ('\r' : toString msg ++ fill)
  atomicModifyIORef' rMaxPrintedSize $ \maxPrinted ->
    (max maxPrinted (length msg), ())
  where
    -- | The maximum possible difference between two progress text representations,
    -- including the timer & the status, is 9 characters. This is a temporary
    -- solution to the problem of re-printing a smaller string on top of another
    -- that'll leave some of the trailing characters in the original string
    -- untouched, and is most likely going to be either replaced by an adequate
    -- workaround or by another way to form a text representation of a progress and
    -- its respective rewriting logic.
    fill = replicate 9 ' '
