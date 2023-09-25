# SPDX-FileCopyrightText: 2023 Serokell <https://serokell.io>
# SPDX-License-Identifier: MPL-2.0

{
  description = "The xrefcheck flake";

  nixConfig.flake-registry = "https://github.com/serokell/flake-registry/raw/master/flake-registry.json";

  outputs = { self, flake-utils, haskell-nix, serokell-nix, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      pkgs = haskell-nix.legacyPackages.${system}.extend serokell-nix.overlay;

      flake = (pkgs.haskell-nix.stackProject {
        src = builtins.path {
          name = "xrefcheck";
          path = ./.;
        };
        modules = [{
          packages.xrefcheck = {
            ghcOptions =
              [ "-Werror" ];

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
      }).flake { crossPlatforms = p: [ p.musl64 p.mingwW64 ]; };

    in
      pkgs.lib.lists.foldr pkgs.lib.recursiveUpdate {} [
        { inherit (flake) packages apps; }
        {
          legacyPackages = pkgs;

          apps.default = self.apps.${system}."x86_64-unknown-linux-musl:xrefcheck:exe:xrefcheck";

          packages = {
            default = self.packages.${system}.xrefcheck;

            xrefcheck = self.packages.${system}."xrefcheck:exe:xrefcheck";

            xrefcheck-static = self.packages.${system}."x86_64-unknown-linux-musl:xrefcheck:exe:xrefcheck";

            xrefcheck-windows = self.packages.${system}."x86_64-w64-mingw32:xrefcheck:exe:xrefcheck";

            docker-image =
            let
              executable = self.packages.${system}.xrefcheck-static;
              binOnly = pkgs.runCommand "xrefcheck-bin" {} ''
                mkdir -p $out/bin
                cp ${executable}/bin/xrefcheck $out/bin
                ${pkgs.nukeReferences}/bin/nuke-refs $out/bin/xrefcheck
              '';
            in pkgs.dockerTools.buildImage {
              name = "xrefcheck";
              contents = [ binOnly pkgs.cacert ];
              config.Entrypoint = "xrefcheck";
            };
          };

          checks = {
            trailing-whitespace = pkgs.build.checkTrailingWhitespace ./.;
            reuse-lint = pkgs.build.reuseLint ./.;
            shellcheck = pkgs.build.shellcheck ./.;
            hlint = pkgs.build.haskell.hlint ./.;
            stylish-haskell = pkgs.build.haskell.stylish-haskell ./.;

            test = self.packages.${system}."xrefcheck:test:xrefcheck-tests";
          };
        }
      ]
    );
}
