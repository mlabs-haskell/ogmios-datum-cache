# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Changed

- Logic of the fetching process changed: always works, can be restated from a specific block, datumFilter can be changed during work.
- Reworked control endpoints:
  - Remove: `POST /control/fetch_blocks`
  - Remove: `POST /control/cancel_fetch_blocks`
  - Add: `POST /control/startingBlock`
  - Add: `POST /control/datumFilter`
- Reworked WebSocket API:
  - Remove WS method: `StartFetchBlocks`
  - Remove WS method: `CancelFetchBlocks`
  - Add WS method:  `SetStartingBlock`
  - Add WS method: `SetDatumFilter`
- Replace configuration file by command line arguments. The config example as CLI: `ogmios-datum-cache --db-connection 'host=localhost port=5432 user=aske dbname=ogmios-datum-cache' --server-port 9999 --server-api 'usr:pwd' --ogmios-address '127.0.0.1' --ogmios-port 1337 --block-slot 44366242 --block-hash d2a4249fe3d0607535daa26caf12a38da2233586bc51e79ed0b3a36170471bf5  --use-latest --block-filter '{"all": [{"hash": "foobar"}, {"any": [{ "address": "addr_abc" },{ "address": "addr_xyz" }]}]}'`
- Control API requires an authentification token (`--server-api`):
  - for endpoints by Basic Authorisation
  - for WebSocket by `token` args.

### Added

- Add `--from-tip` option to cache from current tip.
- Add `--from-origin` option to cache datums of all blocks.
- Add `--log-level` option to enable control of logging.
- Add support for Vasil hardfork (require ogmios `>= 5.5.0`). To work with ogmios `< 5.5.0` should pass `--old-ogmios`.
- Add `GET /healthcheck` endpoint.
- Add `GetHealthcheck` WebSocket method.
- Add `GET /tx/<txId>` endpoint.
- Add `GetTxByHash` WebSocket method.
- Add presets for testing environments `test-env`.
- Add NixOS module for ogmios-datum-cache service. See [nix/test-nixos-configuration.nix](nix/test-nixos-configuration.nix)

### Fixed
- Improve stability of block fetching process.

### Removed
- Configuration file, use command-line arguments instead.

## [0.1.0.0 - 78cf9dbe18da0801a348d3cb3ca9cae3adbbe0d5] - 2022-05-06
Initial version

[Unreleased]: https://github.com/mlabs-haskell/ogmios-datum-cache/compare/78cf9db...master 
