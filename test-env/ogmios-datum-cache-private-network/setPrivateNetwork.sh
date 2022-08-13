
REPO_PATH=$1
ROOT=$2
INIT_SUPPLY=$3
privatePath="./cardano-private-testnet-setup" 
echo "$privatePath"
if [ ! -d $privatePath ]
then 
  echo "copying caradano-private-testnet-setup to $privatePath"
  cp -r ${REPO_PATH} "$privatePath"
  chmod -R +w "$privatePath"
fi
cd "$privatePath"
printf "ROOT=$ROOT\nINIT_SUPPLY=$INIT_SUPPLY" > scripts/config.cfg
(./scripts/automate.sh | while read -r line; do echo "[Setup Cardano node] $line";done) & (echo "[CARDANO NODE] waiting 20 secs to begin node outputs"; sleep 20;echo "[CARDANO NODE] begin cardano node output"; tail -f logs/mainnet.log)
