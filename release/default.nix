# SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

{ pkgs ? import (import ../nix/sources.nix).nixpkgs { } }:
let
  executable = (import ../crossref-verifier.nix {
    static = true;
  }).components.exes.crossref-verify;
  joinByBasename = name: paths:
    pkgs.runCommandNoCC name { } (''
      mkdir $out
    '' + pkgs.lib.concatMapStrings (path: ''
      ln -s ${path} $out/${builtins.baseNameOf path}
    '') paths);
in joinByBasename "crossref-verifier-release" [
  "${executable}/bin"
  ../LICENSES
  ../README.md
]
