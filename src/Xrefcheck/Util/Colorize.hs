{- SPDX-FileCopyrightText: 2018-2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Xrefcheck.Util.Colorize
  ( ColorMode(..)
  , Color(..)
  , Style(..)
  , colorizeIfNeeded
  , colorIfNeeded
  , styleIfNeeded
  ) where

import Universum

import Data.Reflection (Given (..))
import Fmt (Buildable (build), Builder, fmt)
import System.Console.Pretty (Color (..), Pretty (..), Section, Style (..))

{-# HLINT ignore  "Avoid style function that ignore ColorMode" #-}
{-# HLINT ignore  "Avoid color function that ignore ColorMode"#-}
{-# HLINT ignore  "Avoid colorize function that ignore ColorMode"#-}

data ColorMode = WithColors | WithoutColors

instance Pretty Builder where
    colorize s c = build @Text . colorize s c . fmt
    style s = build @Text . style s . fmt

colorIfNeeded :: (Pretty a, Given ColorMode) => Color -> a -> a
colorIfNeeded = case given of
  WithColors -> color
  WithoutColors -> const id

styleIfNeeded :: (Pretty a, Given ColorMode) => Style -> a -> a
styleIfNeeded = case given of
  WithColors -> style
  WithoutColors -> const id

colorizeIfNeeded :: (Pretty a, Given ColorMode) => Section -> Color -> a -> a
colorizeIfNeeded section = case given of
  WithColors -> colorize section
  WithoutColors -> const id
