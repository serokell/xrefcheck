# SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

{ pkgs ? import (import ../nix/sources.nix).nixpkgs { }, timestamp }:
let
  xrefcheck-x86_64-linux =
    (import ../xrefcheck.nix { linux-static = true; }).components.exes.xrefcheck;

  xrefcheck-x86_64-windows =
    (import ../xrefcheck.nix { windows = true; }).components.exes.xrefcheck;

  mkZip = { name, paths, compression ? 5 }:
    pkgs.stdenvNoCC.mkDerivation {
      inherit name;
      buildInputs = [ pkgs.zip ];
      buildCommand = ''
        mkdir -p "$out"
        zip "$out/$name.zip" -jr ${toString paths} -${toString compression}
      '';
    };

  xrefcheck-x86_64-windows-zip = mkZip {
    name = "xrefcheck-x86_64-windows";
    paths = [ "${xrefcheck-x86_64-windows}/bin" ];
  };

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
    path = "${xrefcheck-x86_64-linux}/bin/xrefcheck";
  }
  {
    name = "xrefcheck-x86_64-windows.zip";
    path = "${xrefcheck-x86_64-windows-zip}/xrefcheck-x86_64-windows.zip";
  }
]
