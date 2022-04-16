{ compiler    ? "ghc8107"
, doBenchmark ? false
, doTracing   ? false
, doStrict    ? false
, rev         ? "faad370edcb37162401be50d45526f52bb16a713"
, sha256      ? "1d82d4vh0layf6n925j0h2nym16jbvcvps3l5m8ln9hxn0m6gadn"
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

  modifier = drv: pkgs.haskell.lib.overrideCabal
    (pkgs.haskell.lib.justStaticExecutables drv) (attrs: {
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
