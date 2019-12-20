# SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs (import sources."haskell.nix");
  hn = nixpkgs.haskell-nix;
  project = hn.stackProject {
    src = hn.haskellLib.cleanGit { src = ./.; };
    modules = [{
      packages.crossref-verifier = {
        # More failures during CI == Less failures in runtime!
        postHaddock = ''
          [[ -z "$(ls -A dist/doc/html)" ]] && exit 1 || echo "haddock successfully generated documentation"'';
        package.ghcOptions = "-Werror";
      };
    }];
    cache = with sources; [{
      name = "loot-prelude";
      inherit (lootbox) sha256 rev;
      url = "https://github.com/${lootbox.owner}/${lootbox.repo}.git";
      subdir = "code/prelude";
    }];
  };
in project.crossref-verifier
