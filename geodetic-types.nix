{ mkDerivation, base, dimensional, lens, semigroups, stdenv }:
mkDerivation {
  pname = "geodetic-types";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [ base dimensional lens semigroups ];
  homepage = "https://github.com/qfpl/geodetic-types";
  description = "Types for geodetic operations";
  license = stdenv.lib.licenses.bsd3;
}
