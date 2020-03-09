# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

{ static ? false }:
let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs (import sources."haskell.nix");
  pkgs = if static then nixpkgs.pkgsCross.musl64 else nixpkgs;
  project = pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    modules = [{
      packages.xrefcheck = {
        package.ghcOptions = "-Werror";
        configureFlags = with pkgs;
          lib.optionals static [
            "--ghc-option=-optl=-L${zlib.static}/lib"
          ];
      };
    }];
  };
in project.xrefcheck
