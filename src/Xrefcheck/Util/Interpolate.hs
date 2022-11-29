{- SPDX-FileCopyrightText: 2018-2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Xrefcheck.Util.Interpolate
  ( -- $notes
    interpolateIndentF
  , interpolateBlockListF
  , interpolateBlockListF'
  , interpolateUnlinesF
  )
  where

import Universum

import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (fromLazyText, toLazyText)
import Fmt (Buildable, Builder, blockListF, blockListF', indentF, unlinesF)

{- $notes
The `blockListF` and `indentF` frunctions from @fmt@ add a trailing newline, which makes them unsuitable for string interpolation.
Consider this case:
> [int||
> aaa
> #{indentF 2 "bbb"}
> ccc
> |]
One would reasonably expect this to produce:
> aaa
>   bbb
> ccc
But, in reality, it produces:
> aaa
>   bbb
>
> ccc
This module introduces versions of these functions that do not produce a trailing newline
and can therefore be safely used in string interpolation.
-}

{-# HLINT ignore "Avoid functions that generate extra trailing newlines/whitespaces" #-}

-- | Like @Fmt.indentF@, but strips trailing spaces and does not add a trailing newline.
--
-- >>> import Fmt
-- >>> indentF 2 "a\n\nb"
-- "  a\n  \n  b\n"
--
-- >>> interpolateIndentF 2 "a\n\nb"
-- "  a\n\n  b"
interpolateIndentF :: HasCallStack => Int -> Builder -> Builder
interpolateIndentF n b = (case TL.last (toLazyText b) of
  '\n' -> id
  _ ->  stripLastNewline) $ stripTrailingSpaces $ indentF n b
  -- strips newline added by indentF

-- | Like @Fmt.blockListF'@, but strips trailing spaces and does not add a trailing newline.
interpolateBlockListF' :: HasCallStack => Text -> (a -> Builder) -> NonEmpty a -> Builder
interpolateBlockListF' =  stripLastNewline . stripTrailingSpaces ... blockListF'

-- | Like @Fmt.blockListF@, but strips trailing spaces and does not add a trailing newline.
interpolateBlockListF :: HasCallStack => Buildable a => NonEmpty a -> Builder
interpolateBlockListF = stripLastNewline . stripTrailingSpaces . blockListF

-- | Like @Fmt.unlinesF@, but strips trailing spaces and does not add a trailing newline.
interpolateUnlinesF :: HasCallStack => Buildable a => NonEmpty a -> Builder
interpolateUnlinesF = stripLastNewline . stripTrailingSpaces . unlinesF

-- remove trailing whitespace from all lines.
-- Note: output always ends with newline (adds trailing newline if there wasn't one).
stripTrailingSpaces :: Builder -> Builder
stripTrailingSpaces
  = fromLazyText
  . TL.unlines
  . map (TL.stripEnd)
  . TL.lines
  . toLazyText

stripLastNewline :: HasCallStack => Builder -> Builder
stripLastNewline
  = fromLazyText
  . fromMaybe (error "stripLastNewline: expected newline to strip")
  . TL.stripSuffix "\n"
  . toLazyText
