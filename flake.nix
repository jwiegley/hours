{
  description = "Time tracking software";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      flake = pkgs.hours.flake {
      };
      overlays = [ haskellNix.overlay
        (final: prev: {
          hours =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc963";
              shell.tools = {
                cabal = {};
                haskell-language-server = {};
                hlint = {};
              };
              shell.buildInputs = with pkgs; [
                pkg-config
              ];
            };
        })
      ];
    in {
      packages.default = flake.packages."hours:exe:hours";
      devShell = flake.devShell // {
        withHoogle = true;
      };
    });
}
