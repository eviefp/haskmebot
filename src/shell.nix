let
  inherit (import ../.) pkgs ghcide hlint;
in
  pkgs.mkShell rec {
    nativeBuildInputs = with pkgs; [
      cabal2nix
      haskell.compiler.ghc883
      haskellPackages.ghcid
      haskellPackages.floskell
      ghcide
      hlint
    ];
    NIX_PATH = "nixpkgs=${pkgs.path}";
  }
