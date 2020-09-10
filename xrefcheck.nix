# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

{ linux ? false, linux-static ? false, windows ? false }:
let
  sources = import ./nix/sources.nix;
  haskell-nix = import sources."haskell.nix" {
    sourcesOverride = { hackage = sources."hackage.nix"; stackage = sources."stackage.nix"; };
  };
  nixpkgs = import sources.nixpkgs haskell-nix.nixpkgsArgs;
  pkgs = if linux-static then nixpkgs.pkgsCross.musl64 else if windows then nixpkgs.pkgsCross.mingwW64 else nixpkgs;
  project = pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    modules = [{
      packages.xrefcheck = {
        package.ghcOptions = "-Werror";
      };
    }];
  };
in project.xrefcheck
