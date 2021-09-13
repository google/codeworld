# https://input-output-hk.github.io/haskell.nix/tutorials/getting-started/
let
  sources = import ./nix/sources.nix {};
  haskellNix = import sources.haskellNix {};
  pkgs = import
    haskellNix.sources.nixpkgs-2009
    haskellNix.nixpkgsArgs;
in pkgs.pkgsCross.ghcjs.haskell-nix.project {
  projectFileName = "cabal.project";
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "haskell-nix-project";
    src = ./.;
  };
  compiler-nix-name = "ghc8107";
}
