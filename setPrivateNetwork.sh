#!/bin/sh

locationAtStart=$(pwd)

REPOSRC="https://github.com/woofpool/cardano-private-testnet-setup"
LOCALREPO="test-env/ogmios-datum-cache-private-network/cardano-private-testnet-setup"

mkdir -p $LOCALREPO

# We do it this way so that we can abstract if from just git later on
LOCALREPO_VC_DIR=$LOCALREPO/.git

if [ ! -d $LOCALREPO_VC_DIR ]
then
    git clone $REPOSRC $LOCALREPO
fi

sleep 1

cd $LOCALREPO
./scripts/automate.sh

