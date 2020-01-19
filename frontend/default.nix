{ mkDerivation, base, ghcjs-base, miso, stdenv }:
mkDerivation {
  pname = "frontend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ghcjs-base miso ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
