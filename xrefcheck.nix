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
        ghcOptions =
          [ "-Werror" ];

        components.tests = {
          links-tests = {
            build-tools = [ pkgs.vsftpd ];
            preCheck = ''
              echo "Starting vsftpd..."
              touch /tmp/vsftpd.log
              vsftpd \
              -orun_as_launching_user=yes \
              -olisten_port=2221 \
              -olisten=yes \
              -oftp_username=$(whoami) \
              -oanon_root=${./links-tests/ftp_root} \
              -opasv_min_port=2222 \
              -ohide_file='{.*}' \
              -odeny_file='{.*}' \
              -oseccomp_sandbox=no \
              -olog_ftp_protocol=yes \
              -oxferlog_enable=yes \
              -ovsftpd_log_file=/tmp/vsftpd.log &
              sleep 1
              tail -f /tmp/vsftpd.log &
            '';
            testFlags = [ "--ftp-host" "ftp://localhost:2221" ];
          };
        };
      };
    }];
  };
in
project.xrefcheck
