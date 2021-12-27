{ mkDerivation, aeson, base, base16, bytestring, conduit
, cryptohash-sha256, directory, esqueleto, exceptions, filepath
, http-types, lib, monad-logger, mtl, optparse-applicative
, persistent, persistent-postgresql, resource-pool, servant
, servant-multipart, servant-server, text, time, wai, wai-extra
, wai-logger, warp, websockets
}:
mkDerivation {
  pname = "ogmios-datum-cache";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base base16 bytestring conduit cryptohash-sha256 directory
    esqueleto exceptions filepath http-types monad-logger mtl
    optparse-applicative persistent persistent-postgresql resource-pool
    servant servant-multipart servant-server text time wai wai-extra
    wai-logger warp websockets
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
