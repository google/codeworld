# https://input-output-hk.github.io/haskell.nix/tutorials/getting-started/
{ compiler ? "ghc884",
  ghcjs ? "ghcjs",
  withCoverage ? false
}:
  let
    sources = import ./nix/sources.nix {};
    haskellNix = import sources.haskellNix {};
    pkgs = import
      haskellNix.sources.nixpkgs-2105
      haskellNix.nixpkgsArgs;
  in
    pkgs.pkgsCross.ghcjs.haskell-nix.project {
      projectFileName = "cabal.project";
      src = pkgs.haskell-nix.haskellLib.cleanGit {
        name = "codeworld";
        src = ./.;
      };
      compiler-nix-name = compiler;
    }
