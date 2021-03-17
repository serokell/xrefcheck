# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

{ linux ? false, linux-static ? false, windows ? false }:
let
  nixpkgs = (import ./ci.nix).pkgs;
  src = (import ./ci.nix).project-src;
  pkgs = if linux-static then nixpkgs.pkgsCross.musl64 else if windows then nixpkgs.pkgsCross.mingwW64 else nixpkgs;
  project = pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    modules = [{
      packages.xrefcheck = {
        ghcOptions = [ "-Werror" ];
      };
    }];
  };
in project.xrefcheck
