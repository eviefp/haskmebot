let
  inherit (import ../..) pkgs gitignoreSource;
in
  pkgs.haskell.packages.ghc883.callCabal2nix "twitchbot" (gitignoreSource ./.) {}
