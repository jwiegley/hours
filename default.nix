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
}:

cabal.mkDerivation (self: {
  pname = "hours";
  version = "2.0";
  src = ./.;
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
  ];
  meta = {
    homepage = "https://github.com/jwiegley/hours";
    description = "Compute remaining work hours for a given month";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
