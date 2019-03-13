-- | Printing progress bars.
module Crv.Progress
    ( -- * Progress
      Progress (..)
    , initProgress
    , incProgress
    , incProgressErrors
    , showProgress

      -- * Printing
    , Rewrite
    , allowRewrite
    , putTextRewrite
    ) where

import Data.Ratio ((%))
import GHC.IO.Handle.Text (hPutStr)
import System.Console.Pretty (Color (..), Style (..), color, style)
import Time (ms, threadDelay)

-----------------------------------------------------------
-- Progress
-----------------------------------------------------------

-- | Processing progress of any thing.
data Progress a = Progress
    { pCurrent
      -- ^ How much has been completed.
    , pTotal
      -- ^ Overall amount of work.
    , pErrors :: !a
      -- ^ How many of the completed work finished with an error.
    } deriving (Show)

-- | Initialise null progress.
initProgress :: Num a => a -> Progress a
initProgress a = Progress{ pTotal = a, pCurrent = 0, pErrors = 0 }

-- | Increase progress amount.
incProgress :: (Num a, Show a) => Progress a -> Progress a
incProgress Progress{..} = Progress{ pCurrent = pCurrent + 1, .. }

-- | Increase errors amount.
incProgressErrors :: (Num a, Show a) => Progress a -> Progress a
incProgressErrors Progress{..} = Progress{ pErrors = pErrors + 1, .. }

-- | Visualise progress bar.
showProgress :: Text -> Int -> Color -> Progress Int -> Text
showProgress name width col Progress{..} = mconcat
    [ color col (name <> ": [")
    , toText bar
    , color col "]"
    , status
    ]
  where
    done = floor $ (pCurrent % pTotal) * fromIntegral width
    errs = ceiling $ (pErrors % pTotal) * fromIntegral width
    done' = max 0 $ done - errs
    remained' = width - errs - done'
    bar | pTotal == 0 = replicate width '-'
        | otherwise = mconcat
            [ color Red $ replicate errs '■'
            , color col $ replicate done' '■'
            , color col $ replicate remained' ' '
            , " "
            ]
    status
        | pTotal == 0 = ""
        | pErrors == 0 = style Faint $ color White "✓"
        | otherwise = color Red "!"

-----------------------------------------------------------
-- Rewritable output
-----------------------------------------------------------

-- | Dummy datatype which allows to return caret and replace text in line.
-- Only functions which has this thing can do that because being
-- interleaved with 'putTextLn' printing caret symbol produced garbage.
data Rewrite = Rewrite
    { rMaxPrintedSize :: IORef Int
    }

-- | Provide context for rewrite operations.
allowRewrite :: (MonadIO m, MonadMask m) => (Rewrite -> m a) -> m a
allowRewrite action =
    bracket prepare erase action
  where
    prepare = do
        rMaxPrintedSize <- newIORef 0
        return Rewrite{..}
    erase Rewrite{..} = liftIO $ do
        maxPrintedSize <- readIORef rMaxPrintedSize
        hPutStr stderr $ '\r' : replicate maxPrintedSize ' ' ++ "\r"
        -- prevent our output to interleave with further outputs
        threadDelay (ms 100)

-- | Return caret and print the given text.
putTextRewrite :: MonadIO m => Rewrite -> Text -> m ()
putTextRewrite Rewrite{..} msg = do
    liftIO $ hPutStr stderr ('\r' : toString msg)
    atomicModifyIORef' rMaxPrintedSize $ \maxPrinted ->
        (max maxPrinted (length msg), ())
