#!/bin/sh

REPOSRC="git@github.com:woofpool/cardano-private-testnet-setup.git"
PRIVATE_NETWORK_PATH="test-env/ogmios-datum-cache-private-network"
LOCALREPO="$PRIVATE_NETWORK_PATH/cardano-private-testnet-setup"


WAIT_TIME=5

mkdir -p $LOCALREPO

if [ ! -d "$LOCALREPO/.git" ]
then
    git clone $REPOSRC $LOCALREPO
fi

cd $LOCALREPO

./$PRIVATE_NETWORK_PATH/postgres/startDB.sh "$PRIVATE_NETWORK_PATH/postgres" 5555 odcUser

./scripts/automate.sh $1  && (sleep WAIT_TIME; tail -f /logs/mainnet.log)
