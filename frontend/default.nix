{ mkDerivation, base, containers, ghcjs-base, lens, masse-prelude
, miso, stdenv, text
}:
mkDerivation {
  pname = "frontend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers ghcjs-base lens masse-prelude miso text
  ];
  executableHaskellDepends = [ base masse-prelude ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
