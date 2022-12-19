{- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Xrefcheck.Data.Redirect
  ( RedirectChain
  , RedirectChainLink (..)
  , emptyChain
  , pushRequest
  , hasRequest
  , totalFollowed

  , RedirectRule (..)
  , RedirectRuleOn (..)
  , RedirectRuleOutcome (..)
  , redirectRule

  , isPermanentRedirectCode
  , isRedirectCode
  , isTemporaryRedirectCode
  ) where

import Universum

import Data.Aeson (genericParseJSON)
import Data.Yaml (FromJSON (..), withText)
import Fmt (Buildable (..))
import Text.Regex.TDFA.Text (Regex)

import Data.Sequence ((|>))
import Xrefcheck.Scan ()
import Xrefcheck.Util

-- | A custom redirect rule.
data RedirectRule = RedirectRule
  { rrFrom :: Maybe Regex
    -- ^ Redirect source links that match to apply the rule.
    --
    -- 'Nothing' matches any link.
  , rrTo :: Maybe Regex
    -- ^ Redirect target links that match to apply the rule.
    --
    -- 'Nothing' matches any link.
  , rrOn :: Maybe RedirectRuleOn
    -- ^ HTTP code selector to apply the rule.
    --
    -- 'Nothing' matches any code.
  , rrOutcome :: RedirectRuleOutcome
    -- ^ What to do when an HTTP response matches the rule.
  } deriving stock (Generic)

-- | Rule selector depending on the response HTTP code.
data RedirectRuleOn
    = RROCode Int
      -- ^ An exact HTTP code
    | RROPermanent
      -- ^ Any HTTP code considered as permanent according to 'isPermanentRedirectCode'
    | RROTemporary
      -- ^ Any HTTP code considered as permanent according to 'isTemporaryRedirectCode'
    deriving stock (Show, Eq)

-- | What to do when receiving a redirect HTTP response.
data RedirectRuleOutcome
    = RROValid
      -- ^ Consider it as valid
    | RROInvalid
      -- ^ Consider it as invalid
    | RROFollow
      -- ^ Try again by following the redirect
    deriving stock (Show, Eq)

-- | Links in a redirection chain.
newtype RedirectChain = RedirectChain
  { unRedirectChain :: Seq RedirectChainLink
  } deriving newtype (Show, Eq)

-- | A single link in a redirection chain.
newtype RedirectChainLink = RedirectChainLink
  { unRedirectChainLink :: Text
  } deriving newtype (Show, Eq)

instance FromList RedirectChain where
  type ListElement RedirectChain = Text
  fromList = RedirectChain . fromList . fmap RedirectChainLink

emptyChain :: RedirectChain
emptyChain = RedirectChain mempty

pushRequest :: RedirectChain -> RedirectChainLink -> RedirectChain
pushRequest (RedirectChain chain) = RedirectChain . (chain |>)

hasRequest :: RedirectChain -> RedirectChainLink -> Bool
hasRequest (RedirectChain chain) = (`elem` chain)

totalFollowed :: RedirectChain -> Int
totalFollowed = length . unRedirectChain

instance Buildable RedirectChain where
  build (RedirectChain linksStack) = build chainText
    where
      link (True, RedirectChainLink l) = "-| " <> l
      link (False, RedirectChainLink l) = "-> " <> l

      chainText = mconcat
        $ intersperse "\n"
        $ fmap link
        $ zip (True : repeat False)
        $ toList linksStack

-- | Redirect rule to apply to a link when it has been responded with a given
-- HTTP code.
redirectRule :: Text -> Text -> Int -> [RedirectRule] -> Maybe RedirectRule
redirectRule source target code rules =
  find (matchRule source target code) rules

-- | Check if a 'RedirectRule' matches a given link and HTTP code.
matchRule :: Text -> Text -> Int -> RedirectRule -> Bool
matchRule source target code RedirectRule{..} = and
  [ matchCode
  , matchLink source rrFrom
  , matchLink target rrTo
  ]
  where
    matchCode = case rrOn of
      Nothing -> True
      Just RROPermanent -> isPermanentRedirectCode code
      Just RROTemporary -> isTemporaryRedirectCode code
      Just (RROCode other) -> code == other

    matchLink link = \case
      Nothing -> True
      Just regex -> doesMatchAnyRegex link [regex]

isRedirectCode :: Int -> Bool
isRedirectCode code = code >= 300 && code < 400

isTemporaryRedirectCode :: Int -> Bool
isTemporaryRedirectCode = flip elem [302, 303, 307]

isPermanentRedirectCode :: Int -> Bool
isPermanentRedirectCode = flip elem [301, 308]

instance FromJSON (RedirectRule) where
  parseJSON = genericParseJSON aesonConfigOption

instance FromJSON (RedirectRuleOutcome) where
  parseJSON = withText "Redirect rule outcome" $
    \case
      "valid" -> pure RROValid
      "invalid" -> pure RROInvalid
      "follow" -> pure RROFollow
      _ -> fail "expected (valid|invalid|follow)"

instance FromJSON (RedirectRuleOn) where
  parseJSON v = code v
    <|> text v
    <|> fail "expected a redirect (3XX) HTTP code or (permanent|temporary)"
    where
      code cv = do
        i <- parseJSON cv
        guard $ isRedirectCode i
        pure $ RROCode i
      text = withText "Redirect rule on" $
        \case
          "permanent" -> pure RROPermanent
          "temporary" -> pure RROTemporary
          _ -> mzero
