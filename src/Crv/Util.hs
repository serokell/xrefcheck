{-# OPTIONS_GHC -fno-warn-orphans #-}

module Crv.Util
    ( nameF'
    , paren
    ) where

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
