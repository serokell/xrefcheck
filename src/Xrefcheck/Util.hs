{- SPDX-FileCopyrightText: 2018-2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Xrefcheck.Util
    ( nameF'
    , paren
    , postfixFields
    , (-:)
    , aesonConfigOption
    ) where

import Control.Lens (LensRules, lensField, lensRules, mappingNamer)
import qualified Data.Aeson as Aeson
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Fmt (Builder, build, fmt, nameF)
import System.Console.Pretty (Pretty (..), Style (Faint))

instance Pretty Builder where
    colorize s c = build @Text . colorize s c . fmt
    style s = build @Text . style s . fmt

nameF' :: Builder -> Builder -> Builder
nameF' a b = nameF (style Faint a) b

paren :: Builder -> Builder
paren a
    | a == "" = ""
    | otherwise = "(" <> a <> ")"

postfixFields :: LensRules
postfixFields = lensRules & lensField .~ mappingNamer (\n -> [n ++ "L"])

infixr 0 -:
(-:) :: a -> b -> (a, b)
(-:) = (,)

-- | Options that we use to derive JSON instances for config types.
aesonConfigOption :: Aeson.Options
aesonConfigOption = aesonPrefix camelCase
