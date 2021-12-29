{ mkDerivation, aeson, base, base16, bytestring, conduit
, containers, cryptohash-sha256, directory, esqueleto, exceptions
, filepath, hasql, hasql-th, http-types, lib, monad-logger, mtl
, network, optparse-applicative, persistent, persistent-postgresql
, resource-pool, servant, servant-multipart, servant-server, text
, time, unordered-containers, wai, wai-extra, wai-logger, warp
, websockets
}:
mkDerivation {
  pname = "ogmios-datum-cache";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base base16 bytestring conduit containers cryptohash-sha256
    directory esqueleto exceptions filepath hasql hasql-th http-types
    monad-logger mtl network optparse-applicative persistent
    persistent-postgresql resource-pool servant servant-multipart
    servant-server text time unordered-containers wai wai-extra
    wai-logger warp websockets
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
