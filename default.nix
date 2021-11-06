# https://input-output-hk.github.io/haskell.nix/tutorials/getting-started/
{ compiler ? "ghc865",
  # ghcjs ? "ghcjs",
  withCoverage ? false
}:
  let
    sources = import ./nix/sources.nix {};
    haskellNix = import sources.haskellNix {};
    pkgs = import
      haskellNix.sources.nixpkgs-unstable
      haskellNix.nixpkgsArgs;
      overlays = [
        (self: super:
          {
            m = self.stdenv.mkDerivation {
              name = "m";
              unpackPhase = "true";
              installPhase = "mkdir -p $out";
            };
          }
        )
      ];

  in
    pkgs.haskell-nix.project {
      projectFileName = "cabal.project";
      src = pkgs.haskell-nix.haskellLib.cleanGit {
        name = "codeworld";
        src = ./.;
      };
      compiler-nix-name = compiler;
    }
