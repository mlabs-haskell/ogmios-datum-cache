# adapted from https://github.com/woofpool/cardano-private-testnet-setup/blob/main/6-RUN_PLUTUS_SCRIPT_TXS.md

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd "$SCRIPT_DIR"

ROOT="cardano-private-testnet-setup/private-testnet"
export CARDANO_NODE_SOCKET_PATH=$ROOT/node-bft1/node.sock

if [ ! -d  out ] 
then 
  rm out/*;
else 
  mkdir -p out/;
fi

# generate validator script address and output to file
cardano-cli address build \
--payment-script-file script/validator.plutus \
--testnet-magic 42 \
--out-file out/script.addr

# set a variable to hold the tx_in UTXO value for user1
# query the utxo info for user1
cardano-cli query utxo --address $(cat ${ROOT}/addresses/user1.addr) --testnet-magic 42

# choose a UTXO to assign to tx_in, 
# e.g. tx_in=b0f91ee59eb208284467b1dec0adfa8c57eb1cf7587fb7eb0599e2b8c8e885c9#0

lastLine=$(cardano-cli query utxo --address $(cat ${ROOT}/addresses/user1.addr) --testnet-magic 42 | head -n 3 | tail -1)
array=($lastLine)
tx_in="${array[0]}#${array[1]}"

# Build a transaction to transfer 20 ADA from user1 to the script address and return any change to user1
cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 42 \
    --change-address $(cat "$ROOT/addresses/user1.addr") \
    --tx-in "$tx_in" \
    --tx-out "$(cat out/script.addr) 200000000 lovelace" \
    --tx-out-datum-hash-file script/datum.json \
    --out-file out/tx.body

# The output should be something like this: Estimated transaction fee: Lovelace 297

# sign the transaction with user1 signing key 
cardano-cli transaction sign \
    --tx-body-file out/tx.body \
    --signing-key-file "$ROOT/addresses/user1.skey" \
    --testnet-magic 42 \
    --out-file out/tx.signed

# submit the transaction
cardano-cli transaction submit \
    --testnet-magic 42 \
    --tx-file out/tx.signed

sleep 2
# you should see a UTXO with 20 ADA in value
# set a variable to hold the UTXO value at the script address
# query the UTXO information if necessary
lastLine=$(cardano-cli query utxo --address $(cat out/script.addr) --testnet-magic 42 | tail -1)
array=($lastLine)
script_tx_in="${array[0]}#${array[1]}"
echo "~~~~~~~~~~~~~~~~~~~~~$lastLine~~~~~~~~~~~~~~~~~~~~"

# set a variable to hold the UTXO value for User2, which will be used for collateral
# query the UTXO information if necessary
lastLine=$(cardano-cli query utxo --address $(cat ${ROOT}/addresses/user1.addr) --testnet-magic 42 | head -3 | tail -1)
array=($lastLine)
collateral_tx_in="${array[0]}#${array[1]}"


# set variable with the user2 public key hash from your notes document
signer_pkh="$ROOT/addresses/user1.skey"

# build transaction to let user2 grab the UTXO at the script address
# pass the serialized Plutus script, datum & redeemer inputs, and provide signer hash  
cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 42 \
    --change-address $(cat $ROOT/addresses/user1.addr) \
    --tx-in "$script_tx_in" \
    --tx-in-script-file script/validator.plutus \
    --tx-in-datum-file script/datum.json \
    --tx-in-redeemer-file script/redeemer.json \
    --tx-in-collateral "$collateral_tx_in" \
    --required-signer $signer_pkh \
    --protocol-params-file protocol-parameters.json \
    --out-file out/tx.body

# sign the transaction with user2 signing key
cardano-cli transaction sign \
    --tx-body-file out/tx.body \
    --signing-key-file $ROOT/addresses/user1.skey \
    --testnet-magic 42 \
    --out-file out/tx.signed

# submit the transaction
cardano-cli transaction submit \
    --testnet-magic 42 \
    --tx-file out/tx.signed

echo "waiting 2 secs to confirm transaction"
sleep 2
# wait for a bit to make sure the transaction has been added to the blockchain
# query the UTXOs at user2 address to make sure the grab is successful
cardano-cli query utxo --address $(cat $ROOT/addresses/user1.addr) --testnet-magic 42
