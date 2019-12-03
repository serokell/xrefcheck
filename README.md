# Crossref-verifier

Tool for verifying local and external references in repository documentation.

For now, support for markdown files only is provided.

## Build instructions [↑](#crossref-verifier)

Run `stack build` to build everything.

## Usage [↑](#crossref-verifier)

To find all broken links in repository run

```sh
stack exec crossref-verify -- --root <repository>
```

By default, only repo-local references are verified; to include references to external resources provide corresponding value for `--mode` parameter:

```sh
stack exec crossref-verify -- -root <repository> --mode full
```

## Issue tracker [↑](#crossref-verifier)

We use [YouTrack](https://issues.serokell.io/issues/INT) as our issue
tracker. You can login using your GitHub account to leave a comment or
create a new issue.

## For Contributors [↑](#crossref-verifier)

Please see [CONTRIBUTING.md](/.github/CONTRIBUTING.md) for more information.

## About Serokell [↑](#crossref-verifier)

Crossref-verifier is maintained by [Serokell](https://serokell.io/).

We love open source software.
See which [services](https://serokell.io/#services) we provide and [drop us a line](mailto:hi@serokell.io) if you are interested.
