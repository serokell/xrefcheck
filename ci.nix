# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

rec {
  sources = import ./nix/sources.nix;
  haskell-nix = import sources."haskell.nix" {
    sourcesOverride = { hackage = sources."hackage.nix"; stackage = sources."stackage.nix"; };
  };
  serokell-nix = import sources."serokell.nix";
  pkgs = import sources.nixpkgs (
    haskell-nix.nixpkgsArgs // {
      overlays =
        haskell-nix.nixpkgsArgs.overlays
        ++ [ serokell-nix.overlay ]; # contains trailing whitespace check
    }
  );

  project-src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "xrefcheck";
    src = ./.;
  };

  xrefcheck-lib-and-tests = (import ./xrefcheck.nix { linux = true; });
  xrefcheck-static = (import ./xrefcheck.nix { linux-static = true; }).components.exes.xrefcheck;
  xrefcheck-windows = (import ./xrefcheck.nix { windows = true; }).components.exes.xrefcheck;

  trailing-whitespace-check = pkgs.build.checkTrailingWhitespace project-src;
}
