{ compiler    ? "ghc865"
, doBenchmark ? false
, doTracing   ? false
, doStrict    ? false
, rev         ? "2cd2e7267e5b9a960c2997756cb30e86f0958a6b"
, sha256      ? "0ir3rk776wldyjz6l6y5c5fs8lqk95gsik6w45wxgk6zdpsvhrn5"
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
    # hpack no longer builds with 8.6.5, but we only need the executable, not
    # a library.
    hpack           = pkgs.haskell.packages.ghc883.hpack;
    rebase          = doJailbreak super.rebase;
    time-recurrence = markUnbroken (doJailbreak super.time-recurrence);
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
