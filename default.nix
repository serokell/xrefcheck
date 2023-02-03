# SPDX-FileCopyrightText: 2023 Serokell <https://serokell.io>
# SPDX-License-Identifier: MPL-2.0

let
  inherit (import
  (let lock = builtins.fromJSON (builtins.readFile ./flake.lock); in
    fetchTarball {
      url = "https://github.com/edolstra/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
      sha256 = lock.nodes.flake-compat.locked.narHash;
    })
  { src = ./.; }) defaultNix;
  system = builtins.currentSystem;
in defaultNix //
   (defaultNix.legacyPackages.${system}.lib.attrsets.mapAttrs (_: val: val.${system}) defaultNix)
