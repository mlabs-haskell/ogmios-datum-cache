# Environment for integral tests with vasil network

Configuration: https://book.world.dev.cardano.org/environments.html#vasil-dev

## Prerequisite

- docker
- docker-compose

## Starting up a development environment

``` shell
$ cd test-env/ogmios-datum-cache-vasil-testnet
$ mkdir node-db node-ipc ogmios-datum-cache-postgresql
$ docker-compose up
```

If you already have synced node-db, you need to update volume section in `docker-compose.yml`.

## Check node

``` shellsession
$ cd test-env/ogmios-datum-cache-vasil-testnet
$ export CARDANO_NODE_SOCKET_PATH=$PWD/node-ipc/node.socket
$ sudo chmod 0666 $CARDANO_NODE_SOCKET_PATH
$ cardano-cli query tip --testnet-magic 9
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
$ cabal run ogmios-datum-cache --disable-optimisation -- --db-connection 'host=localhost port=5432 user=aske dbname=ogmios-datum-cache' --server-port 9999 --server-api 'usr:pwd' --ogmios-address '127.0.0.1' --ogmios-port 1337 --origin --use-latest 
Build profile: -w ghc-8.10.7 -O0
In order, the following will be built (use -v for more details):
 - ogmios-datum-cache-0.1.0.0 (exe:ogmios-datum-cache) (file app/Main.hs changed)
...
```

## DB inspection

``` shellsession
$ psql -U aske -d ogmios-datum-cache -h localhost
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
