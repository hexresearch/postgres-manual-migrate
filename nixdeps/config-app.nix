{ mkDerivation, aeson, base, directory, fetchgit, filepath
, interpolatedstring-perl6, optparse-applicative, stdenv, uniplate
, yaml
}:
mkDerivation {
  pname = "config-app";
  version = "0.1.0.2";
  src = fetchgit {
    url = "https://github.com/hexresearch/config-app.git";
    sha256 = "0bq91awb16ykmhr74dfki6g19qng2ys82cnn3gd1i0l4dndipkdc";
    rev = "a23131bd4337ef493a24e24f9934158de44a7db4";
  };
  libraryHaskellDepends = [
    aeson base directory filepath interpolatedstring-perl6
    optparse-applicative uniplate yaml
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/hexresearch/config-app#readme";
  description = "Constructor for CLI apps with YAML-config file";
  license = stdenv.lib.licenses.bsd3;
}
