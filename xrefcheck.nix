# SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

{ static ? true }:
let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs (import sources."haskell.nix");
  hn = if static then
    nixpkgs.pkgsCross.musl64.haskell-nix
  else
    nixpkgs.haskell-nix;
  project = hn.stackProject {
    src = hn.haskellLib.cleanGit { src = ./.; };
    modules = [{
      packages.xrefcheck = {
        # More failures during CI == Less failures in runtime!
        postHaddock = ''
          [[ -z "$(ls -A dist/doc/html)" ]] && exit 1 || echo "haddock successfully generated documentation"'';
        package.ghcOptions = "-Werror";
        components.exes.xrefcheck.configureFlags =
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
    cache = with sources; [{
      name = "loot-prelude";
      inherit (lootbox) sha256 rev;
      url = "https://github.com/${lootbox.owner}/${lootbox.repo}.git";
      subdir = "code/prelude";
    }];
  };
in project.xrefcheck
