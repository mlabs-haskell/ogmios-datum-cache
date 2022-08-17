
ROOT="cardano-private-testnet-setup/private-testnet"
export CARDANO_NODE_SOCKET_PATH=$ROOT/node-bft1/node.sock
ADDR=user2

# payment address keys
cardano-cli address key-gen \
--verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
--signing-key-file      ${ROOT}/addresses/${ADDR}.skey

# stake address keys
cardano-cli stake-address key-gen \
--verification-key-file ${ROOT}/addresses/${ADDR}-stake.vkey \
--signing-key-file      ${ROOT}/addresses/${ADDR}-stake.skey

# wallet address
cardano-cli address build \
--payment-verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
--stake-verification-key-file ${ROOT}/addresses/${ADDR}-stake.vkey \
--testnet-magic 42 \
--out-file ${ROOT}/addresses/${ADDR}.addr  

# stake address
cardano-cli stake-address build \
--stake-verification-key-file ${ROOT}/addresses/${ADDR}-stake.vkey \
--testnet-magic 42 \
--out-file ${ROOT}/addresses/${ADDR}-stake.addr

# stake addresses registration cert
cardano-cli stake-address registration-certificate \
--stake-verification-key-file ${ROOT}/addresses/${ADDR}-stake.vkey \
--out-file ${ROOT}/addresses/${ADDR}-stake.reg.cert

cardano-cli query protocol-parameters \
--testnet-magic 42 \
--out-file protocol-parameters.json

cardano-cli query utxo --address $(cat ${ROOT}/addresses/user1.addr) --testnet-magic 42
