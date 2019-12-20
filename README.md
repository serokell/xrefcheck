<!--
 - SPDX-FileCopyrightText: 2018-2019 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -->

# Crossref-verifier

[![Build status](https://badge.buildkite.com/75461331a6058b334383cdfca1071dc1f908b70cf069d857b7.svg?branch=master)](https://buildkite.com/serokell/crossref-verifier)

Crossref-verifier is a tool for verifying local and external references in repository documentation that is quick, easy to setup, and suitable to be added to CI.

<img src="https://user-images.githubusercontent.com/5394217/70820564-06b06e00-1dea-11ea-9680-27f661ca2a58.png" alt="Output sample" width="600"/>

### Motivation

As the project evolves, links in documentation have a tendency to get broken. This is usually because of: 
1. File movements;
2. Markdown header renames;
3. Outer sites ceasing their existence.

This tool will help you to keep references in order.

### Aims

Comparing to alternative solutions, this tool tries to achieve the following points:

* Quickness - local references are verified instantly even for moderately-sized repositories.
* Easy setup - no extra actions required, just run the tool in the repository root.
Both relative and absolute local links are supported out of the box.
* Conservative verifier allows using this tool in CI, no false positives (e.g. on sites which require authentication) should be reported.

### A comparison with other solutions

* [linky](https://github.com/mattias-p/linky) - a well-configurable verifier written in Rust, scans one file at a time and works good in pair with system utilities like `find`.
  This tool requires some configuring before it can be applied to a repository or added to CI.
* [awesome_bot](https://github.com/dkhamsing/awesome_bot) - a solution written in Ruby that can be easily included in CI or integrated into GitHub.
  Its features include duplicated URLs detection, specifying allowed HTTP error codes and reporting generation.
  At the moment of writting, it scans only external references and checking anchors is not possible.
* [remark-validate-links](https://github.com/remarkjs/remark-validate-links) and [remark-lint-no-dead-urls](https://github.com/davidtheclark/remark-lint-no-dead-urls) - highly configurable Javascript solution for checking local and remote links resp.
  It is able to check multiple repositores at once if they are gathered in one folder.
  Being written on JavaScript, it is fairly slow on large repositories.
* [markdown-link-check](https://github.com/tcort/markdown-link-check) - another checker written in JavaScript, scans one file at a time.
  Supports `mailto:` link resolution.
* [url-checker](https://github.com/paramt/url-checker) - GitHub action which checks links in specified files.
* [broken-link-checker](https://github.com/stevenvachon/broken-link-checker) - advanced checker for `HTML` files.

## Build instructions [↑](#crossref-verifier)

Run `stack install` to build everything and install the executable.

### CI and nix [↑](#crossref-verifier)

To build only the executables, run `nix-build`. You can use this line on your CI to use crossref-verifier:
```
nix run -f https://github.com/serokell/crossref-verifier/archive/master.tar.gz -c crossref-verify
```

Our CI uses `nix-build crossref-verifier.nix` to build the whole project, including tests and Haddock.
It is based on the [`haskell.nix`](https://input-output-hk.github.io/haskell.nix/) project.
You can do that too if you wish.

<details>
  <summary>Details</summary>

There is a [bug](https://github.com/input-output-hk/haskell.nix/issues/335) which causes us to put some redundancy into Nix files:
1. [`nix/sources.json`](nix/sources.json) lists all such dependencies that we obtain using `git`.
It specifies concrete git revisions and SHA256 checksums.
2. [`crossref-verifier.nix`](crossref-verifier.nix) lists all such dependencies as well, but without revisions.

As a consequence, you may have to update these files when you update [`stack.yaml`](stack.yaml).
You can use [`niv update`](https://github.com/nmattia/niv#update) to update [`nix/sources.json`](nix/sources.json).

</details>

## Usage [↑](#crossref-verifier)

To find all broken links in a repository, run from within its folder:

```sh
crossref-verify
```

To also display all found links and anchors:

```sh
crossref-verify -v
```

For description of other options:

```sh
crossref-verify --help
```

## Configuring

Configuration template (with all options explained) can be dumped with:

```sh
crossref-verify dump-config
```

Currently supported options include:
* Timeout for checking external references;
* List of ignored folders.

## For further work [↑](#crossref-verifier)

- [ ] Support for non-Unix systems.
- [ ] Support link detection in different languages, not only Markdown.
  - [ ] Haskell Haddock is first in turn.

## Issue tracker [↑](#crossref-verifier)

We use GitHub issues as our issue tracker.
You can login using your GitHub account to leave a comment or create a new issue.

## For Contributors [↑](#crossref-verifier)

Please see [CONTRIBUTING.md](/.github/CONTRIBUTING.md) for more information.

## About Serokell [↑](#crossref-verifier)

Crossref-verifier is maintained and funded with ❤️ by [Serokell](https://serokell.io/).
The names and logo for Serokell are trademark of Serokell OÜ.

We love open source software! See [our other projects](https://serokell.io/community?utm_source=github) or [hire us](https://serokell.io/hire-us?utm_source=github) to design, develop and grow your idea!
