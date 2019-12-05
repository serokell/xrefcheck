# Crossref-verifier

[![Build status](https://badge.buildkite.com/75461331a6058b334383cdfca1071dc1f908b70cf069d857b7.svg?branch=master)](https://buildkite.com/serokell/crossref-verifier)

Tool for verifying local and external references in repository documentation.

For now, support for markdown files only is provided.

## Build instructions [↑](#crossref-verifier)

Run `stack install` to build everything and install executable.

### CI and nix [↑](#crossref-verifier)

To build only the executables, run `nix-build`. You can use this line on your CI to use crossref-verifier:
```
nix run -f https://github.com/serokell/crossref-verifier/archive/master.tar.gz -c crossref-verify
```

Our CI uses `nix-build crossref-verifier.nix` to build the whole project including tests and haddock.
It is based on the [`haskell.nix`](https://input-output-hk.github.io/haskell.nix/) project.
You can do that too if you want.

There is a [bug](https://github.com/input-output-hk/haskell.nix/issues/335) which causes us to put some redundancy into Nix files:
1. [`nix/sources.json`](nix/sources.json) lists all such dependencies that we obtain using `git`.
It specifies concrete git revisions and SHA256 checksums.
2. [`crossref-verifier.nix`](crossref-verifier.nix) lists all such dependencies as well, but without revisions.

As a consequence, you may have to update these files when you update [`stack.yaml`](stack.yaml).
You can use [`niv update`](https://github.com/nmattia/niv#update) to update [`nix/sources.json`](nix/sources.json).

## Usage [↑](#crossref-verifier)

To find all broken links in repository run

```sh
crossref-verify --root <repository>
```

For description of other options

```sh
crossref-verify --help
```

## Issue tracker [↑](#crossref-verifier)

We use [YouTrack](https://issues.serokell.io/issues/INT) as our issue
tracker. You can login using your GitHub account to leave a comment or
create a new issue.

## For Contributors [↑](#crossref-verifier)

Please see [CONTRIBUTING.md](/.github/CONTRIBUTING.md) for more information.

## About Serokell [↑](#crossref-verifier)

Crossref-verifier is maintained and funded with ❤️ by [Serokell](https://serokell.io/).
The names and logo for Serokell are trademark of Serokell OÜ.

We love open source software! See [our other projects](https://serokell.io/community?utm_source=github) or [hire us](https://serokell.io/hire-us?utm_source=github) to design, develop and grow your idea!
