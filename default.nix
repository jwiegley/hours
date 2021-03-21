{ compiler    ? "ghc8104"
, doBenchmark ? false
, doTracing   ? false
, doStrict    ? false
, rev         ? "5df05c902cde398e056eb6271d5fe13e418db4c6"
, sha256      ? "12plc7k251z1dmmrd29lyrpw0xmjvmf79yj568aapzrcki5mrw74"
, pkgs        ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = true;
  }
, returnShellEnv ? pkgs.lib.inNixShell
, mkDerivation ? null
}:

let haskellPackages = pkgs.haskell.packages.${compiler};

in haskellPackages.developPackage {
  root = ./.;

  overrides = with pkgs.haskell.lib; self: super: {
    inherit (pkgs.haskell.packages.ghc884) hpack;

    active           = doJailbreak super.active;
    diagrams-contrib = doJailbreak super.diagrams-contrib;
    diagrams-core    = doJailbreak super.diagrams-core;
    diagrams-svg     = doJailbreak super.diagrams-svg;
    dual-tree        = doJailbreak super.dual-tree;
    force-layout     = doJailbreak super.force-layout;
    monoid-extras    = doJailbreak super.monoid-extras;
    # rebase           = doJailbreak super.rebase;
    size-based       = doJailbreak super.size-based;
    statestack       = doJailbreak super.statestack;
    svg-builder      = doJailbreak super.svg-builder;

    diagrams-cairo = unmarkBroken (doJailbreak
      (self.callCabal2nix "diagrams-cairo" (pkgs.fetchFromGitHub {
        owner  = "diagrams";
        repo   = "diagrams-cairo";
        rev    = "533e4f4f18f961543bb1d78493c750dec45fd4a3";
        sha256 = "18z38b8hq0laxd2q458pa58z3ls1fm9l3p09vsi3q8q4605d84k6";
        # date = 2020-02-08T04:32:35-06:00;
      }) {}));

    diagrams-lib = unmarkBroken (doJailbreak
      (self.callCabal2nix "diagrams-lib" (pkgs.fetchFromGitHub {
        owner  = "diagrams";
        repo   = "diagrams-lib";
        rev    = "6f66ce6bd5aed81d8a1330c143ea012724dbac3c";
        sha256 = "0kn3kk8pc7kzwz065g8mpdbsbmbds3vrrgz2215f96ivivv8b9lw";
        # date = 2021-03-02T17:03:02-06:00;
      }) {}));

    time-recurrence = unmarkBroken (doJailbreak
      (self.callCabal2nix "time-recurrence" (pkgs.fetchFromGitHub {
        owner  = "jwiegley";
        repo   = "time-recurrence";
        rev    = "d1771331ffd495035cb7f1b2dd14cdf86b11d2fa";
        sha256 = "1l9vf5mzq2r22gph45jk1a4cl8i53ayinlwq1m8dbx3lpnzsjc09";
        # date = 2021-03-21T14:27:27-07:00;
      }) {}));
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
