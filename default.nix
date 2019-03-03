{ compiler    ? "ghc844"
, doBenchmark ? false
, doTracing   ? false
, doStrict    ? false
, rev         ? "120eab94e0981758a1c928ff81229cd802053158"
, sha256      ? "0qk6k8gxx5xlkyg05dljywj5wx5fvrc3dzp4v2h6ab83b7zwg813"
, pkgs        ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = false;
  }
, returnShellEnv ? pkgs.lib.inNixShell
, mkDerivation ? null
}:

let haskellPackages = pkgs.haskell.packages.${compiler};

in haskellPackages.developPackage {
  root = ./.;

  overrides = with pkgs.haskell.lib; self: super: {
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
    active              = doJailbreak super.active;
    force-layout        = doJailbreak super.force-layout;
    svg-builder         = doJailbreak super.svg-builder;
    these               = doJailbreak super.these;

    aeson               = overrideCabal super.aeson (attrs: {
      libraryHaskellDepends =
        attrs.libraryHaskellDepends ++ [ super.contravariant ];
    });
  };

  source-overrides = {
  };

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    doHaddock = false;

    inherit doBenchmark;

    installPhase = ''
      mkdir -p $out/bin
      cp jobhours $out/bin
      cp gethours $out/bin
      cp dist/build/work-periods/work-periods $out/bin
      cp dist/build/timelog-periods/timelog-periods $out/bin
      cp dist/build/process-hours/process-hours $out/bin
    '';

    # Required for org2tc
    executableSystemDepends = [ pkgs.python ];
  });

  inherit returnShellEnv;
}
