
REPO_PATH=$1
LOCAL_PATH="cardano-private-testnet-setup"


if [ ! -d "$LOCAL_PATH" ]
then
    cp -r $REPO_PATH $LOCAL_PATH
fi

./scripts/automate.sh 
