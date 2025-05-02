{
  description = "Time tracking software";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
    org2tc = {
      url = "git+file:org2tc";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, org2tc }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      flake = pkgs.hours.flake {
      };
      org2tc-pkg = pkgs.callPackage org2tc {};
      jobhours = pkgs.writeScriptBin "jobhours" (builtins.readFile ./jobhours);
      overlays = [ haskellNix.overlay
        (final: prev: {
          hours =
            final.haskell-nix.project' {
              src = ./.;
              supportHpack = true;
              compiler-nix-name = "ghc910";
              shell.tools = {
                cabal = {};
                haskell-language-server = {};
                # hlint = {};
              };
              shell.buildInputs = with pkgs; [
                pkg-config
              ];
            };
        })
      ];
    in {
      packages = {
        all = pkgs.symlinkJoin {
          name = "all";
          paths = builtins.attrValues flake.packages ++ [
            org2tc-pkg
            jobhours
          ];
        };
        default = flake.packages."hours:exe:process-hours";
        work-periods = flake.packages."hours:exe:work-periods";
        timelog-periods = flake.packages."hours:exe:timelog-periods";
        org2tc = org2tc-pkg;
        jobhours = jobhours;
      };

      devShell = flake.devShell // {
        withHoogle = true;
      };
    });
}
