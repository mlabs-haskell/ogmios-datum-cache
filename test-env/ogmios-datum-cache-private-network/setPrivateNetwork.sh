
REPO_PATH=$1
ROOT=$2
INIT_SUPPLY=$3
privatePath="./cardano-private-testnet-setup" 

cardanoNodePrefix="Cardano Node"

RED="31"
PURPLE="35"

function echoWithPrefix() {
  prefix=$1 
  color=$2
  output=$3
  printf "\033[0;${color}m$prefix |\033[0m $output \n" 
}


function runNetwork() {
  ./scripts/automate.sh | (while read -r line; do echoWithPrefix "$cardanoNodePrefix" "$RED" "$line";done)
}

function showNetworkLog() {
  echoWithPrefix "$cardanoNodePrefix" "$RED" "waiting 20 secs to begin node outputs"
  sleep 20
  echoWithPrefix "$cardanoNodePrefix" "$RED" "begin cardano node output"
  tail -f logs/mainnet.log | (while read -r line; do echoWithPrefix "$cardanoNodePrefix" "$RED" "$line";done)

}

function startOgmios() {
  sleep 5
  ogmios --port 1337 --host localhost \
    --node-socket private-testnet/node-bft1/node.sock \
    --node-config private-testnet/configuration.yaml \
    | (while read -r line; do echoWithPrefix "Ogmios" "$PURPLE" "$line";done)

}



if [ ! -d $privatePath ]
then 
  echo "copying caradano-private-testnet-setup to $privatePath"
  cp -r ${REPO_PATH} "$privatePath"
  chmod -R +w "$privatePath"
fi
cd "$privatePath"
printf "ROOT=$ROOT\nINIT_SUPPLY=$INIT_SUPPLY" > scripts/config.cfg

runNetwork & showNetworkLog  & startOgmios

#(./scripts/automate.sh | while read -r line; do echo "[Setup Cardano node] $line";done) & (echo "[CARDANO NODE] waiting 20 secs to begin node outputs"; sleep 20;echo "[CARDANO NODE] begin cardano node output"; tail -f logs/mainnet.log)
