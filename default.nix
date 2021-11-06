# https://input-output-hk.github.io/haskell.nix/tutorials/getting-started/
{ compiler ? "ghc865",
  ghcjsVersion ? "8.6.0.0.10",
  ghcVersion ? "8.6.5",
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
    pkgs.haskell-nix.ghcjsProject {
      # projectFileName = "cabal.project";
      ghcjsVersion = "8.6.0.0.10";
      src = pkgs.haskell-nix.haskellLib.cleanGit {
        name = "codeworld";
        src = ./.;
      };
      compiler-nix-name = compiler;
    }
