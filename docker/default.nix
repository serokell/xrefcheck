# SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

{ pkgs ? import (import ../nix/sources.nix).nixpkgs { } }:
let
  executable =
    (import ../xrefcheck.nix { static = true; }).components.exes.xrefcheck;
  binOnly = pkgs.runCommand "xrefcheck-bin" { } ''
    mkdir -p $out/bin
    cp ${executable}/bin/xrefcheck $out/bin
    ${pkgs.nukeReferences}/bin/nuke-refs $out/bin/xrefcheck
  '';
in pkgs.dockerTools.buildImage {
  name = "xrefcheck";
  contents = [ binOnly pkgs.cacert ];
  config.Entrypoint = "xrefcheck";
}
