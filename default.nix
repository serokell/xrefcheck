let
  sources = import (./nix/sources.nix);
  nixpkgs = import sources.nixpkgs {
    overlays = [ (import "${sources.serokell-closure}/pkgs") ];
  };
  hlib = nixpkgs.haskell.lib;
in (nixpkgs.stackToNix {
  root = nixpkgs.constGitIgnore "crossref-verifier" ./. [ ];
  overrides = (final: previous: {
    tasty-hedgehog = null;
    tiempo = null;
    time-units = null;
    crossref-verifier = hlib.overrideCabal previous.crossref-verifier (o: {
      buildTools = (o.buildTools or [ ]) ++ [ final.hspec-discover ];
      doHaddock = false;
    });
  });
}).crossref-verifier
