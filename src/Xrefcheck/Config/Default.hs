{- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Xrefcheck.Config.Default where

import Universum

import Text.Interpolation.Nyan

import Xrefcheck.Core
import Xrefcheck.Util

defConfigText :: Flavor -> Text
defConfigText flavor =
  [int|D|
# Exclusion parameters.
exclusions:
  # Ignore these files. References to them will fail verification,
  # and references from them will not be verified.
  # List of glob patterns.
  ignore: []

  # References from these files will not be verified.
  # List of glob patterns.
  ignoreRefsFrom:
#{interpolateIndentF 4 $ interpolateBlockListF $ ignoreLocalRefsFrom}

  # References to these paths will not be verified.
  # List of glob patterns.
  ignoreLocalRefsTo:
#{interpolateIndentF 4 $ interpolateBlockListF $ ignoreLocalRefsTo}

  # References to these URIs will not be verified.
  # List of POSIX extended regular expressions.
  ignoreExternalRefsTo:
    # Ignore localhost links by default
    - ^(https?|ftps?)://(localhost|127\\.0\\.0\\.1).*

# Networking parameters.
networking:
  # When checking external references, how long to wait on request before
  # declaring "Response timeout".
  externalRefCheckTimeout: 10s

  # Skip links which return 403 or 401 code.
  ignoreAuthFailures: true

  # When a verification result is a "429 Too Many Requests" response
  # and it does not contain a "Retry-After" header,
  # wait this amount of time before attempting to verify the link again.
  defaultRetryAfter: 30s

  # How many attempts to retry an external link after getting
  # a "429 Too Many Requests" response.
  maxRetries: 3

# Parameters of scanners for various file types.
scanners:
  # On 'anchor not found' error, how much similar anchors should be displayed as
  # hint. Number should be between 0 and 1, larger value means stricter filter.
  anchorSimilarityThreshold: 0.5

  markdown:
    # Flavor of markdown, e.g. GitHub-flavor.
    #
    # This affects which anchors are generated for headers.
    flavor: #s{flavor}

  # Whether the copy-paste check is enabled globally.
  copyPasteCheckEnabled: true
|]
  where
    ignoreLocalRefsFrom :: NonEmpty Text
    ignoreLocalRefsFrom = fromList $  case flavor of
      GitHub ->
        [ ".github/pull_request_template.md"
        , ".github/issue_template.md"
        , ".github/PULL_REQUEST_TEMPLATE/**/*"
        , ".github/ISSUE_TEMPLATE/**/*"
        ]
      GitLab ->
        [ ".gitlab/merge_request_templates/**/*"
        , ".gitlab/issue_templates/**/*"
        ]

    ignoreLocalRefsTo :: NonEmpty Text
    ignoreLocalRefsTo = fromList $  case flavor of
      GitHub ->
        [ "../../../issues"
        , "../../../issues/*"
        , "../../../pulls"
        , "../../../pulls/*"
        ]
      GitLab ->
        [ "../../issues"
        , "../../issues/*"
        , "../../merge_requests"
        , "../../merge_requests/*"
        ]
