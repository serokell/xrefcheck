let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs (import sources."haskell.nix");
  hn = nixpkgs.haskell-nix;
in (hn.stackProject {
  src = hn.haskellLib.cleanGit { src = ./.; };
  cache = [{
    name = "loot-prelude";
    rev = "4700376b8493f6ac164461715cb72a0259148ac2";
    sha256 = "0yh1xi9p7ky5bm40pqjs0ygxqcbds226sj481r1cagnk0lp5vl3f";
    url = "https://github.com/serokell/lootbox";
    subdir = "code/prelude";
  }];
}).crossref-verifier
