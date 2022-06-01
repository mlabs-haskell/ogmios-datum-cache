# Environment for integral tests with test network

## Prerequisite

- docker
- docker-compose

## Starting up a development environment

``` shell
$ pwd
.../ogmios-datum-cache/test-env/ogmios-datum-cache-testnet
$ mkdir node-db node-ipc ogmios-datum-cache-postgresql
$ docker-compose up
```

If you already have synced node-db, you need to update volume section in `docker-compose.yml`.

## Check node

``` shellsession
$ sudo chmod 0666 $PWD/node-ipc/node.socket
$ CARDANO_NODE_SOCKET_PATH=$PWD/node-ipc/node.socket cardano-cli query tip --testnet-magic 1097911063
{
    "block": 1396078,
    "epoch": 64,
    "era": "Byron",
    "hash": "91d74148aa839b0b37a41452f1e5281a964c0bbe08f19c390017c221936ef09a",
    "slot": 1397228,
    "syncProgress": "31.17"
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

## Cleanup

``` shellsession
$ pwd
.../ogmios-datum-cache/test-env/ogmios-datum-cache-testnet
$ docker-compose down --volumes
Removing ogmios-datum-cache-testnet_postgresql_1   ... done
Removing ogmios-datum-cache-testnet_ogmios_1       ... done
Removing ogmios-datum-cache-testnet_cardano-node_1 ... done
Removing network ogmios-datum-cache-testnet_default
Removing volume ogmios-datum-cache-testnet_node-db
Removing volume ogmios-datum-cache-testnet_node-ipc
Removing volume ogmios-datum-cache-testnet_node-config
Removing volume ogmios-datum-cache-testnet_ogmios-datum-cache-db
$ sudo rm -Rf node-db/ node-ipc/ ogmios-datum-cache-postgresql/
```
