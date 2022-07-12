{- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE QuasiQuotes #-}

module Xrefcheck.Config.Default where

import Universum

import Text.RawString.QQ

defConfigUnfilled :: ByteString
defConfigUnfilled =
  [r|# Parameters of repository traversal.
traversal:
  # Files and folders which we pretend do not exist
  # (so they are neither analyzed nor can be referenced).
  ignored:
    # Git files
    - .git

    # Stack files
    - .stack-work

# Verification parameters.
verification:
  # On 'anchor not found' error, how much similar anchors should be displayed as
  # hint. Number should be between 0 and 1, larger value means stricter filter.
  anchorSimilarityThreshold: 0.5

  # When checking external references, how long to wait on request before
  # declaring "Response timeout".
  externalRefCheckTimeout: 10s

  # Prefixes of files, references in which should not be analyzed.
  notScanned:
    - :PLACEHOLDER:notScanned:

  # Glob patterns describing the files which do not physically exist in the
  # repository but should be treated as existing nevertheless.
  virtualFiles:
    - :PLACEHOLDER:virtualFiles:

  # POSIX extended regular expressions that match external references
  # that have to be ignored (not verified).
  # It is an optional parameter, so it can be omitted.
  ignoreRefs: []

  # Check localhost links.
  checkLocalhost: false

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
  markdown:
    # Flavor of markdown, e.g. GitHub-flavor.
    #
    # This affects which anchors are generated for headers.
    flavor: :PLACEHOLDER:flavor:
|]
