# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

rec {
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  xrefcheck-lib-and-tests = (import ./xrefcheck.nix { linux = true; });
  xrefcheck-static = (import ./xrefcheck.nix { linux-static = true; }).components.exes.xrefcheck;
  xrefcheck-windows = (import ./xrefcheck.nix { windows = true; }).components.exes.xrefcheck;
}
