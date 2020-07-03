# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

{ pkgs ? import (import ../nix/sources.nix).nixpkgs { } }:
let
  executable =
    (import ../xrefcheck.nix { linux-static = true; }).components.exes.xrefcheck;
  binOnly = pkgs.runCommand "xrefcheck-bin" { } ''
    mkdir -p $out/bin
    cp ${executable}/bin/xrefcheck $out/bin
    ${pkgs.nukeReferences}/bin/nuke-refs $out/bin/xrefcheck
  '';
in pkgs.dockerTools.buildLayeredImage {
  name = "xrefcheck";
  contents = [ binOnly pkgs.cacert pkgs.pkgsStatic.bash ];
  config.Entrypoint = "xrefcheck";
}
