let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs (import sources."haskell.nix");
  hn = nixpkgs.haskell-nix;
in (hn.stackProject {
  src = hn.haskellLib.cleanGit { src = ./.; };
  cache = with sources; [
    {
      name = "loot-prelude";
      inherit (lootbox) sha256 rev;
      url = "https://github.com/${lootbox.owner}/${lootbox.repo}.git";
      subdir = "code/prelude";
    }
  ];
}).crossref-verifier
