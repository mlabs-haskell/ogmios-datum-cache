#!/bin/sh

REPOSRC="git@github.com:woofpool/cardano-private-testnet-setup.git"
LOCALREPO="cardano-private-testnet-setup"

POSTGRES_PATH="postgres"
POSTGRES_CONFIG="$POSTGRES_PATH/config"
POSTGRES_PORT="5555"
POSTGRES_USER="odcUser"


mkdir -p $LOCALREPO

if [ ! -d "$LOCALREPO/.git" ]
then
    git clone $REPOSRC $LOCALREPO
fi


export PGDATA="$(pwd)/postgres/DB"

mkdir -p "$POSTGRES_PATH/sockets"
./postgres/startDB.sh $POSTGRES_CONFIG $POSTGRES_PORT $POSTGRES_USER

failure=$?

if [ $failure -ne 0 ] 
then 
  echo "Can't set postgres database."
  exit 
fi


./scripts/automate.sh $1  & (sleep WAIT_TIME; tail -f /logs/mainnet.log)
