{- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

module Xrefcheck.Config.Default where

import Universum

import Fmt (Builder)
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
    - (https?|ftps?)://(localhost|127\\.0\\.0\\.1).*

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
  # Timeouts may also be accounted here, see the description
  # of `maxTimeoutRetries` field.

  # If a site once responded with 429 error code, subsequent
  # request timeouts will also be treated as hitting the site's
  # rate limiter and result in retry attempts, unless the
  # maximum retries number has been reached.
  #
  # On other errors xrefcheck fails immediately, without retrying.
  maxRetries: 3

  # Querying a given domain that ever returned 429 before,
  # this defines how many timeouts are allowed during retries.
  #
  # For such domains, timeouts likely mean hitting the rate limiter,
  # and so xrefcheck considers timeouts in the same way as 429 errors.
  #
  # For other domains, a timeout results in a respective error, no retry
  # attempts will be performed. Use `externalRefCheckTimeout` option
  # to increase the time after which timeout is declared.
  #
  # This option is similar to `maxRetries`, the difference is that
  # this `maxTimeoutRetries` option limits only the number of retries
  # caused by timeouts, and `maxRetries` limits the number of retries
  # caused both by 429s and timeouts.
  maxTimeoutRetries: 1

  # Maximum number of links that can be followed in a single redirect
  # chain.
  #
  # The link is considered as invalid if the limit is exceeded.
  maxRedirectFollows: 10

  # Rules to override the redirect behavior for external references that
  # match, where
  #   - 'from' is a regular expression for the source link in a single
  #     redirection step. Its absence means that every link matches.
  #   - 'to' is a regular expression for the target link in a single
  #     redirection step. Its absence also means that every link matches.
  #   - 'on' accepts 'temporary', 'permanent' or a specific redirect HTTP code.
  #     Its absence also means that every response code matches.
  #   - 'outcome' accepts 'valid', 'invalid' or 'follow'. The last one follows
  #      the redirect by applying the same configuration rules so, for instance,
  #      exclusion rules would also apply to the following links.
  #
  # The first one that matches is applied, and the link is considered
  # as valid if none of them does match.
  externalRefRedirects:
#{interpolateIndentF 4 externalRefRedirects}

  # The maximum allowed total size of HTTP headers (in bytes) that can
  # be returned by the server.
  #
  # If the total size of the headers exceeds this value, the request will
  # fail with an error to prevent the processing of excessively large headers.
  maxHeaderLength: 4096

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
|]
  where
    ignoreLocalRefsFrom :: NonEmpty Text
    ignoreLocalRefsFrom = fromList $ case flavor of
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
    ignoreLocalRefsTo = fromList $ case flavor of
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

    externalRefRedirects :: Builder
    externalRefRedirects = case flavor of
      GitHub ->
        [int||
        - on: permanent
          outcome: invalid|]
      GitLab ->
        [int||
        - on: permanent
          outcome: invalid
        # GitLab redirects non-existing files to the repository's main page
        # with a 302 code instead of answering with a 404 response.
        - from: https?://gitlab.com/.*/-/blob/.*
          to: https?://gitlab.com/.*
          on: 302
          outcome: invalid|]
