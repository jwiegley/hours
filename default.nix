{ mkDerivation, attoparsec, base, bytestring, lens, old-locale
, optparse-applicative, shelly, stdenv, text, time, time-recurrence
, timeparsers, transformers
}:
mkDerivation {
  pname = "hours";
  version = "2.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    attoparsec base bytestring lens old-locale optparse-applicative
    shelly text time time-recurrence timeparsers transformers
  ];
  homepage = "https://github.com/jwiegley/hours";
  description = "Tool to print out hours worked toward monthly goal";
  license = stdenv.lib.licenses.bsd3;
}
