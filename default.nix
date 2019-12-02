let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs
    (import sources."haskell.nix");
  hn = nixpkgs.haskell-nix;
in (hn.stackProject {
  src = hn.haskellLib.cleanGit { src = ./.; };
  cache = [
    {
      name = "loot-prelude";
      rev = "34e389808e34f1cfa56f456773682325bed56d17";
      sha256 = "19b51x0yml13wpvj0b967vgg5y62av94izjr9sfwxr6sww2wgk29";
      url = "https://github.com/serokell/lootbox.git";
      subdir = "code/prelude";
    }
  ];
}).crossref-verifier
