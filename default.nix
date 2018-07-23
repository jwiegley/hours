{ compiler    ? "ghc822"
, doBenchmark ? false
, doTracing   ? false
, doStrict    ? false
, rev         ? "49bdae006e66e70ad3245a463edc01b5749250d3"
, sha256      ? "1ijsifmap47nfzg0spny94lmj66y3x3x8i6vs471bnjamka3dx8p"
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
      cp dist/build/bae-periods/bae-periods $out/bin
      cp dist/build/timelog-periods/timelog-periods $out/bin
      cp dist/build/process-hours/process-hours $out/bin
    '';
  });

  inherit returnShellEnv;
}
