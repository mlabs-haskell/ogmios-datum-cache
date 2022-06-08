# Environment for integral tests with vasil network

## Prerequisite

- docker
- docker-compose

## Starting up a development environment

``` shell
$ pwd
.../ogmios-datum-cache/test-env/ogmios-datum-cache-vasil-testnet
$ mkdir node-db node-ipc ogmios-datum-cache-postgresql
$ docker-compose up
```

If you already have synced node-db, you need to update volume section in `docker-compose.yml`.

## Check node

``` shellsession
$ CARDANO_NODE_SOCKET_PATH=$PWD/node-ipc/node.socket cardano-cli query tip --testnet-magic 9
{
    "block": 31723,
    "epoch": 99,
    "era": "Babbage",
    "hash": "ea29cd6ef68bc3e5b2b321fe37155bc2c0438f68629f4bbb88d176d8bfe97c11",
    "slot": 712259,
    "syncProgress": "100.00"
}
```

## Run ogmios-datum-cache

``` shellsession
$ pwd
.../ogmios-datum-cache
$ cabal run ogmios-datum-cache
Build profile: -w ghc-8.10.7 -O0
In order, the following will be built (use -v for more details):
 - ogmios-datum-cache-0.1.0.0 (exe:ogmios-datum-cache) (file app/Main.hs changed)
...
```

## Fetch from the first block

``` shellsession
$ http POST localhost:9999/control/fetch_blocks slot:=0 id="f4ba3c4db14c9e9d0879bae07a249fe7169bff64e357199ffe5179cbfe83b19e"
HTTP/1.1 200 OK
Content-Type: application/json;charset=utf-8
Date: Wed, 08 Jun 2022 08:15:09 GMT
Server: Warp/3.3.17
Transfer-Encoding: chunked

{
    "message": "Started block fetcher"
}

```

## Cleanup

``` shellsession
$ pwd
.../ogmios-datum-cache/test-env/ogmios-datum-cache-vasil-testnet
$ docker-compose down --volumes
Removing ogmios-datum-cache-vasil-testnet_ogmios_1       ... done
Removing ogmios-datum-cache-vasil-testnet_cardano-node_1 ... done
Removing ogmios-datum-cache-vasil-testnet_postgresql_1   ... done
Removing network ogmios-datum-cache-vasil-testnet_default
Removing volume ogmios-datum-cache-vasil-testnet_node-db
Removing volume ogmios-datum-cache-vasil-testnet_node-ipc
Removing volume ogmios-datum-cache-vasil-testnet_node-config
Removing volume ogmios-datum-cache-vasil-testnet_ogmios-datum-cache-db
$ sudo rm -Rf node-db/ node-ipc/ ogmios-datum-cache-postgresql/
```
