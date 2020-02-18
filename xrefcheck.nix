# SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

{ static ? false }:
let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs (import sources."haskell.nix");
  hn = if static then
    nixpkgs.pkgsCross.musl64.haskell-nix
  else
    nixpkgs.haskell-nix;
  project = hn.stackProject {
    src = hn.haskellLib.cleanGit { src = ./.; };
    modules = [
      {
      packages.xrefcheck = {
        package.ghcOptions = "-Werror";
        configureFlags =
          with nixpkgs.pkgsStatic;
          lib.optionals static [
            "--disable-executable-dynamic"
            "--disable-shared"
            "--ghc-option=-optl=-pthread"
            "--ghc-option=-optl=-static"
            "--ghc-option=-optl=-lc"
            "--ghc-option=-optl=-lz"
            "--ghc-option=-optl=-lgmp"
            "--ghc-option=-optl=-lffi"
            "--ghc-option=-optl=-L${gmp6}/lib"
            "--ghc-option=-optl=-L${zlib.static}/lib"
            "--ghc-option=-optl=-L${libffi}/lib"
          ];
      };
    }];
  };
in project.xrefcheck
