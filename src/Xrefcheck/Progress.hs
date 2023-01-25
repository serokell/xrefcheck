{- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

-- | Printing progress bars.
module Xrefcheck.Progress
  ( -- * Task timestamp
    TaskTimestamp (..)

    -- * Progress
  , Progress
  , initProgress
  , initProgressWitnessed
  , reportSuccess
  , reportError
  , reportRetry
  , getTaskTimestamp
  , setTaskTimestamp
  , removeTaskTimestamp
  , checkTaskTimestamp
  , sameProgress
  , showProgress

    -- * Printing
  , Rewrite
  , allowRewrite
  , putTextRewrite
  ) where

import Universum

import Data.Ratio ((%))
import Data.Reflection (Given)
import Data.Set qualified as S
import Time (Second, Time, sec, unTime, (-:-))

import Xrefcheck.Util

-----------------------------------------------------------
-- Task timestamp
-----------------------------------------------------------

-- | Data type defining a point in time when an anonymous task had started
-- and its time to completion.
data TaskTimestamp = TaskTimestamp
  { ttTimeToCompletion :: Time Second
    -- ^ The amount of time required for the task to be completed.
  , ttStart :: Time Second
    -- ^ The timestamp of when the task had started, represented by the number of seconds
    -- since the Unix epoch.
  } deriving stock (Show)

-----------------------------------------------------------
-- Progress
-----------------------------------------------------------

-- | Processing progress of any thing, measured with type @a@, where progress units have witnesses
-- of type @w@ that can be retried.
--
-- The () type can be used as a trivial witness if the retry logic is not going to be used.
data Progress a w = Progress
  { pTotal :: !a
    -- ^ Overall amount of work.
  , pSuccess :: !a
    -- ^ How much has been completed with success.
  , pError :: !a
    -- ^ How much has been completed with error.
  , pRetrying :: !(S.Set w)
    -- ^ Witnesses of items that have been completed with error but are being retried.
  , pTaskTimestamp :: !(Maybe TaskTimestamp)
    -- ^ A timestamp of an anonymous timer task, where its time to completion is
    -- the time needed to pass for the action to be retried immediately after.
  } deriving stock (Show)

-- | Initialise null progress.
initProgress :: Num a => a -> Progress a w
initProgress a = Progress
  { pTotal = a
  , pSuccess = 0
  , pError = 0
  , pRetrying = S.empty
  , pTaskTimestamp = Nothing
  }

-- | Initialise null progress from a given list of witnesses.
--
-- This just initializes it with as many work to do as witnesses are in the list, so you can be
-- more confident regarding the progress initialization because you actualy provided data that
-- represents each unit of work to do.
initProgressWitnessed :: [w] -> Progress Int w
initProgressWitnessed ws = Progress
  { pTotal = length ws
  , pSuccess = 0
  , pError = 0
  , pRetrying = S.empty
  , pTaskTimestamp = Nothing
  }

-- | Report a unit of success with witness @item@.
reportSuccess :: (Num a, Ord w) => w -> Progress a w -> Progress a w
reportSuccess item Progress{..} = Progress
  { pSuccess = pSuccess + 1
  , pRetrying = S.delete item pRetrying
  , ..
  }

-- | Report a unit of failure with witness @item@.
reportError :: (Num a, Ord w) => w -> Progress a w -> Progress a w
reportError item Progress{..} = Progress
  { pError = pError + 1
  , pRetrying = S.delete item pRetrying
  , ..
  }

-- | Report a unit of failure and retry intention with witness @item@.
reportRetry :: Ord w => w -> Progress a w -> Progress a w
reportRetry item Progress{..} = Progress
  { pRetrying = S.insert item pRetrying
  , ..
  }

-- | Set the current `TaskTimestamp`.
--
-- It does require a witness because, although the `TaskTimestamp` is
-- anonymous, at this point an actual task should be responsible for
-- registering this timestamp.
setTaskTimestamp :: w -> Time Second -> Time Second -> Progress a w -> Progress a w
setTaskTimestamp _ ttc startTime Progress{..} = Progress
  { pTaskTimestamp = Just (TaskTimestamp ttc startTime)
  , ..
  }

-- | Get the current `TaskTimestamp`.
--
-- It does not require a witness because the `TaskTimestamp` is anonymous
-- and anyone should be able to observe it.
getTaskTimestamp :: Progress a w -> Maybe TaskTimestamp
getTaskTimestamp = pTaskTimestamp

removeTaskTimestamp :: Progress a w -> Progress a w
removeTaskTimestamp Progress{..} = Progress
  { pTaskTimestamp = Nothing
  , ..
  }

checkTaskTimestamp :: Time Second -> Progress a w -> Progress a w
checkTaskTimestamp posixTime p@Progress{..} =
  case pTaskTimestamp of
    Nothing -> p
    Just TaskTimestamp{..} ->
      if ttTimeToCompletion >= posixTime -:- ttStart
      then p
      else removeTaskTimestamp p

-- | Check whether the two @Progress@ values are equal up to similarity of their essential
-- components, ignoring the comparison of @pTaskTimestamp@s, which is done to prevent test
-- failures when comparing the resulting progress, gotten from running the link
-- verification algorithm, with the expected one, where @pTaskTimestamp@ is hardcoded
-- as @Nothing@.
sameProgress :: (Eq a, Eq w) => Progress a w -> Progress a w -> Bool
sameProgress p1 p2 = and
  [ ((==) `on` pTotal) p1 p2
  , ((==) `on` pSuccess) p1 p2
  , ((==) `on` pError) p1 p2
  , ((==) `on` pRetrying) p1 p2
  ]

-- | Visualise progress bar.
showProgress :: Given ColorMode => Text -> Int -> Color -> Time Second -> Progress Int w -> Text
showProgress name width col posixTime Progress{..} = mconcat
  [ colorIfNeeded col (name <> ": [")
  , toText bar
  , timer
  , colorIfNeeded col "]"
  , status
  ]
  where
    -- Each of the following values represents the number of the progress bar cells
    -- corresponding to the respective "class" of processed references: the valid ones,
    -- the ones containing an unfixable error (a.k.a. the invalid ones), and the ones
    -- containing a fixable error.
    --
    -- The current overall number of proccessed errors.
    done = floor $ (current % pTotal) * fromIntegral @Int @(Ratio Int) width

    -- The current number of the invalid references.
    errsU = ceiling $ (pError % pTotal) * fromIntegral @Int @(Ratio Int) width

    -- The current number of (fixable) errors that may be eliminated during further
    -- verification.
    -- Notice!
    --   1. Both this and the previous values use @ceiling@ as the rounding function.
    --      This is done to ensure that as soon as at least 1 faulty reference occurs during
    --      the verification, the cell of its respective color is mathematically guaranteed
    --      to be visible in the progress bar visualization.
    --   2. @errsF@ is bounded from above by @width - errsU@ to prevent an overflow in the
    --      number of the progress bar cells that could be caused by the two @ceilings@s.
    errsF = min (width - errsU) . ceiling $ (fixable % pTotal) *
      fromIntegral @Int @(Ratio Int) width

    -- The number of valid references.
    -- The value is bounded from below by 0 to ensure the number never gets negative.
    -- This situation is plausible due to the different rounding functions used for each value:
    -- @floor@ for the minuend @done@, @ceiling@ for the two subtrahends @errsU@ & @errsF@.
    successful = max 0 $ done - errsU - errsF

    -- The remaining number of references to be verified.
    remaining = width - successful - errsU - errsF

    bar
      | pTotal == 0 = replicate width '-'
      | otherwise = mconcat
          [ colorIfNeeded Blue $ replicate errsF '■'
          , colorIfNeeded Red $ replicate errsU '■'
          , colorIfNeeded col $ replicate successful '■'
          , colorIfNeeded col $ replicate remaining ' '
          , " "
          ]
    timer = case pTaskTimestamp of
      Nothing -> ""
      Just TaskTimestamp{..} -> mconcat
        [ colorIfNeeded col "|"
        , colorIfNeeded Blue . show . timeSecondCeiling
        $ ttTimeToCompletion -:- (posixTime -:- ttStart)
        ]
    status = mconcat
      [ if current == pTotal && fixable == 0 && pError == 0
        then styleIfNeeded Faint $ colorIfNeeded White "✓"
        else ""
      , if fixable /= 0 then colorIfNeeded Blue "!" else ""
      , if pError /= 0 then colorIfNeeded Red "!" else ""
      ]

    timeSecondCeiling :: Time Second -> Time Second
    timeSecondCeiling = sec . fromInteger . ceiling . unTime

    fixable :: Int
    fixable = S.size pRetrying

    current :: Int
    current = pSuccess + pError + fixable

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
    -- The maximum possible difference between two progress text representations,
    -- including the timer & the status, is 9 characters. This is a temporary
    -- solution to the problem of re-printing a smaller string on top of another
    -- that'll leave some of the trailing characters in the original string
    -- untouched, and is most likely going to be either replaced by an adequate
    -- workaround or by another way to form a text representation of a progress and
    -- its respective rewriting logic.
    fill = replicate 9 ' '
