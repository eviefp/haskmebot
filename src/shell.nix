let
  tooling = import ../default.nix;
  secrets = import ./secrets.nix;
  self = tooling.haskell;
in
  tooling.pkgs.mkShell {
      buildInputs = [
        self.ghc883
        self.haskell-language-server
        self.stylish-haskell
        self.hlint
        self.cabal-install
      ];
      HASKMEBOT_PASS = secrets.oauth;
  }
