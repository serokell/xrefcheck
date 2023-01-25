# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

src:
pkgs:
let
  project = pkgs.haskell-nix.stackProject {
    inherit src;
    modules = [{
      packages.xrefcheck = {
        ghcOptions = [ "-Werror" ];

        components.tests = {
          ftp-tests = {
            build-tools = [ pkgs.vsftpd ];
            preCheck = ''
              echo "Starting vsftpd..."
              touch /tmp/vsftpd.log
              vsftpd \
              -orun_as_launching_user=yes \
              -olisten_port=2221 \
              -olisten=yes \
              -oftp_username=$(whoami) \
              -oanon_root=${./ftp-tests/ftp_root} \
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
          xrefcheck-tests.build-tools = [ pkgs.git ];
        };
      };
    }];
  };
in project.xrefcheck
