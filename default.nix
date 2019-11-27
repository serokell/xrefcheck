let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs
    # (import sources."haskell.nix");
    (import ../../haskell.nix);
  hn = nixpkgs.haskell-nix;
in (hn.stackProject {
  src = hn.haskellLib.cleanGit { src = ./.; };
  cache = [
    {
      name = "lootbox";
      type = "stack";
      rev = "34e389808e34f1cfa56f456773682325bed56d17";
      sha256 = "19b51x0yml13wpvj0b967vgg5y62av94izjr9sfwxr6sww2wgk29";
      url = "https://github.com/serokell/lootbox.git/";
    }
    {
      name = "caps";
      rev = "6938f8dcefa1aa8aa7d7f272eb0828a3da463671";
      sha256 = "1rdz8vzb0gwnwkn0a028l4rwd3c7cqqcajr46sp9jas5i1nhmccp";
      url = "https://github.com/int-index/caps/";
    }
  ];
}).crossref-verifier
