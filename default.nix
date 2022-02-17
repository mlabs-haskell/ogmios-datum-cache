{ mkDerivation, aeson, async, base, base16, base64, bytestring
, cborg, cborg-json, co-log, conduit, containers, cryptohash-sha256
, directory, esqueleto, exceptions, filepath, hasql, http-types
, lib, monad-logger, mtl, network, optparse-applicative, persistent
, persistent-postgresql, resource-pool, serialise, servant
, servant-multipart, servant-server, servant-websockets, text, time
, tomland, unliftio, unliftio-core, unordered-containers, vector
, wai, wai-cors, wai-extra, wai-logger, warp, websockets
}:
mkDerivation {
  pname = "ogmios-datum-cache";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base base16 base64 bytestring cborg cborg-json co-log
    conduit containers cryptohash-sha256 directory esqueleto exceptions
    filepath hasql http-types monad-logger mtl network
    optparse-applicative persistent persistent-postgresql resource-pool
    serialise servant servant-multipart servant-server
    servant-websockets text time tomland unliftio unliftio-core
    unordered-containers vector wai wai-cors wai-extra wai-logger warp
    websockets
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
