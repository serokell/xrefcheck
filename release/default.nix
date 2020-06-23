# SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

{ pkgs ? import (import ../nix/sources.nix).nixpkgs { }, timestamp }:
let
  executable =
    (import ../xrefcheck.nix { static = true; }).components.exes.xrefcheck;

  releaseNotes = pkgs.writeText "release-notes.md" ''
    Automatic release on ${timestamp}
  '';
in pkgs.linkFarm "xrefcheck-release" [
  {
    name = "release-notes.md";
    path = releaseNotes;
  }
  {
    name = "xrefcheck-x86_64-linux";
    path = "${executable}/bin/xrefcheck";
  }
]
