# SPDX-FileCopyrightText: 2023 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

{
  nixConfig.flake-registry =
    "https://github.com/serokell/flake-registry/raw/master/flake-registry.json";

  inputs = {
    haskell-nix.inputs = {
      hackage.follows = "hackage";
      stackage.follows = "stackage";
    };

    hackage.flake = false;

    stackage.flake = false;
  };
  outputs = { self, nixpkgs, flake-utils, haskell-nix, serokell-nix, hackage
    , stackage }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system}.extend
          (nixpkgs.lib.composeManyExtensions [
            serokell-nix.overlay
            haskell-nix.overlay
          ]);
        project-src = pkgs.haskell-nix.haskellLib.cleanSourceWith {
          filter = path: type: !(pkgs.lib.hasInfix "tests/golden/helpers" path);
          src = ./.;
        };
        xrefcheck = import ./xrefcheck.nix project-src;
      in {
        legacyPackages = { xrefcheck-lib-and-tests = xrefcheck pkgs; };
        packages = {
          default =
            self.legacyPackages.${system}.xrefcheck-lib-and-tests.components.exes.xrefcheck;
          xrefcheck-static =
            (xrefcheck pkgs.pkgsStatic).components.exes.xrefcheck;
          xrefcheck-windows =
            (xrefcheck pkgs.pkgsCross.mingwW64).components.exes.xrefcheck;
        };
        checks =
          self.legacyPackages.${system}.xrefcheck-lib-and-tests.components.tests
          // {
            trailing-whitespace-check =
              pkgs.build.checkTrailingWhitespace project-src;
          };

      });
}
