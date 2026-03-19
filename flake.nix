{
  description = "Time tracking and budget analysis tool";

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
                supportHpack = true;
                compiler-nix-name = "ghc910";
                shell = {
                  tools = {
                    cabal = {};
                    haskell-language-server = {};
                  };
                  buildInputs = with pkgs; [
                    pkg-config
                    haskellPackages.fourmolu
                    haskellPackages.hlint
                    lefthook
                    shellcheck
                  ];
                  withHoogle = true;
                };
              };
          })
        ];
      in flake // {
        packages.default = flake.packages."hours:exe:process-hours";
        devShells.default = flake.devShells.default;

        checks = flake.checks // {
          formatting = pkgs.runCommand "check-formatting" {
            nativeBuildInputs = [ pkgs.haskellPackages.fourmolu ];
          } ''
            cd ${self}
            find . -name '*.hs' \
              -not -path './dist-*' \
              -not -path './.git/*' \
              | xargs fourmolu --mode check
            touch $out
          '';

          linting = pkgs.runCommand "check-linting" {
            nativeBuildInputs = [ pkgs.haskellPackages.hlint ];
          } ''
            cd ${self}
            hlint src/ work/ timelog/ Main.hs test/
            touch $out
          '';

          shellcheck-scripts = pkgs.runCommand "check-shellcheck" {
            nativeBuildInputs = [ pkgs.shellcheck ];
          } ''
            cd ${self}
            shellcheck jobhours gethours scripts/*.sh
            touch $out
          '';
        };
      });
}
