<!--
 - SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -->

Unreleased
==========
* [#117](https://github.com/serokell/xrefcheck/pull/117)
  + Forbid verifying a single file using `--root` command line option.
* [#115](https://github.com/serokell/xrefcheck/pull/115)
  + Improved parsing of anchor html tags inside headers.
* [#109](https://github.com/serokell/xrefcheck/pull/109)
  + Fixed bug when prefixing `--ignored` and `--root` paths with `./`
* [#85](https://github.com/serokell/xrefcheck/pull/85)
  + Make possible to specify whether ignore localhost links, use
  `check-localhost` CLA argument (by default localhost links will not be checked).
  + Make possible to ignore auth failures (assume 'protected' links
  valid), use `ignoreAuthFailures` parameter of config.
* [#66](https://github.com/serokell/xrefcheck/pull/66)
  + Added support for ftp links.
* [#74](https://github.com/serokell/xrefcheck/pull/83)
  + Add the duplication detection & verification result caching algorithm for external references.
* [#82](https://github.com/serokell/xrefcheck/pull/82)
  + Fix the issue of having the lowest level context duplicated, caused by the root's trailing path separator.
* [#31](https://github.com/serokell/xrefcheck/pull/88)
  + Handle the "429 too many requests" errors & attempt to eliminate them during verification.

0.2.1
==========
* [#68](https://github.com/serokell/xrefcheck/pull/68)
  + Recognise manual HTML-anchors inside headers.

0.2
==========
* [#57](https://github.com/serokell/xrefcheck/pull/57)
  + Added `flavor` field to config.
    Also see [config sample](tests/configs/github-config.yaml).
  + Config generated with `dump-config` CLI command now depends on the provided repository type.

0.1.3
=======

* [#58](https://github.com/serokell/xrefcheck/pull/58)
  + Switch to lts-17.3.
* [#53](https://github.com/serokell/xrefcheck/pull/53)
  + Make possible to include a regular expression in
  `ignoreRefs` parameter of config to ignore external
  references.
  + Add support of right in-place ignoring annotations
  such as `ignore file`, `ignore paragraph` and `ignore link`.

0.1.2
=======

* [#44](https://github.com/serokell/xrefcheck/pull/44)
  + Decide whether to show progress bar by default depending on `CI` env variable.
  + Added `--progress` option.

0.1.1.2
=======

* [#34](https://github.com/serokell/xrefcheck/pull/34)
  + Do not depend on `autoexporter` and `base-noprelude`.

0.1.1.1
=======

* [#30](https://github.com/serokell/xrefcheck/pull/32)
  + Do not depend on `loot-prelude` package.

0.1.1
=======

* [#19](https://github.com/serokell/xrefcheck/pull/24)
  + Make `ignored` in config consider only exact matches.
  + Improve virtual files consideration.
  + Add `ignored` CLI option.

0.1.0.0
=======

Initial release.
