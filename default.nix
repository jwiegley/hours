{ compiler    ? "ghc822" # "ghc842" also works
, doProfiling ? false
, doBenchmark ? false
, doTracing   ? false
, doStrict    ? false
, rev         ? "9d0b6b9dfc92a2704e2111aa836f5bdbf8c9ba42"
, sha256      ? "096r7ylnwz4nshrfkh127dg8nhrcvgpr69l4xrdgy3kbq049r3nb"
, nixpkgs     ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = false;
  }
, provideDrv  ? !nixpkgs.pkgs.lib.inNixShell
}:

let inherit (nixpkgs) pkgs;

  haskellPackages' = pkgs.haskell.packages.${compiler};

  haskellPackages = pkgs.lib.fix (this: haskellPackages'.override {
    overrides = with pkgs.haskell.lib; self: super: {
      developPackage =
        { root
        , source-overrides ? {}
        , overrides ? self: super: {}
        , modifier ? drv: drv
        , provideDrv ? !pkgs.lib.inNixShell }:
        let drv =
          (this.extend
             (pkgs.lib.composeExtensions
                (self.packageSourceOverrides source-overrides)
                overrides))
          .callCabal2nix (builtins.baseNameOf root) root {};
        in if provideDrv then modifier drv else (modifier drv).env;

      time-recurrence     = doJailbreak super.time-recurrence;
      diagrams-builder    = doJailbreak super.diagrams-builder;
      diagrams-cairo      = doJailbreak super.diagrams-cairo;
      diagrams-contrib    = doJailbreak super.diagrams-contrib;
      diagrams-core       = doJailbreak super.diagrams-core;
      diagrams-graphviz   = doJailbreak super.diagrams-graphviz;
      diagrams-lib        = doJailbreak super.diagrams-lib;
      diagrams-postscript = doJailbreak super.diagrams-postscript;
      diagrams-rasterific = doJailbreak super.diagrams-rasterific;
      diagrams-svg        = doJailbreak super.diagrams-svg;
      circle-packing      = doJailbreak super.circle-packing;
    };
  });

in haskellPackages.developPackage {
  root = ./.;

  source-overrides = {
  };

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    enableLibraryProfiling    = doProfiling;
    enableExecutableProfiling = doProfiling;

    inherit doBenchmark;
  });

  inherit provideDrv;
}
