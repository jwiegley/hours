{ cabal
, attoparsec
, bytestring ? null
, lens
, oldLocale ? null
, optparseApplicative
, shelly
, text
, time
, timeRecurrence
, timeparsers
, transformers
}:

cabal.mkDerivation (self: {
  pname = "hours";
  version = "2.0";
  src = builtins.filterSource (path: type: type != "unknown") ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    attoparsec
    bytestring
    lens
    oldLocale
    optparseApplicative
    shelly
    text
    time
    timeRecurrence
    timeparsers
    transformers
  ];
  meta = {
    homepage = "https://github.com/jwiegley/hours";
    description = "Compute remaining work hours for a given month";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
