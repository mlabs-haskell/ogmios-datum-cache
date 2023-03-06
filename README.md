# ogmios-datum-cache

# :rotating_light: DEPRECATION NOTICE :rotating_light:

As of Mar 05 2023, this repository has been archived and will no longer receive updates and support. See [kupo](https://github.com/CardanoSolutions/kupo) for alternative indexer that supports everything ODC used to.

## Datum query

### Plutus `Data` in JSON

```haskell
data Data =
      Constr Integer [Data]
    | Map [(Data, Data)]
    | List [Data]
    | I Integer
    | B BS.ByteString
    deriving stock (Show, Eq, Ord, Generic)

instance ToJSON Data where
  toJSON = \case
    Constr n ds -> object [ "constr" .= n, "fields" .= ds ]
    Map ds -> object [ "map" .= map (\(k, v) -> object [ "key" .= k, "value" .= v ]) ds ]
    List ds -> toJSON ds
    I int -> toJSON int
    B bs -> toJSON $ BSBase16.encodeBase16 bs
```


### `GET /datum/<hash>`

Request: `GET /datum/179f56ecaaad4a92dd6554aaeaac7089dc4bd9903ffb047c28d75da90fe3f259`

Response:
```jsonc
{
  "constr": 0,
  "fields": [
    {
      "constr": 0,
      "fields": [
        "16289cc42ac5087215488ef9db05a06dd8bf2e338dc53e9f6c80705d",
        1638113842637
      ]
    },
    {
      "constr": 0,
      "fields": [
        {
          "constr": 0,
          "fields": [
            "",
            ""
          ]
        },
        {
          "constr": 0,
          "fields": [
            "648823ffdad1610b4162f4dbc87bd47f6f9cf45d772ddef661eff198",
            "76425443"
          ]
        },
        190
      ]
    }
  ]
}
```

### `GET /datums`

Request:
```jsonc
{
  "hashes": [
    "e827cc9fab9038391dabbe6b79440d7a14c4a38de5a69b2e130acbb46b5ae6ed",
    "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"
  ]
}
```

Response
```jsonc
{
  "datums": [
    {
      "hash": "e827cc9fab9038391dabbe6b79440d7a14c4a38de5a69b2e130acbb46b5ae6ed",
      "value": {
        "constr": 0,
        "fields": [
          "737461746530",
          1638684000,
          42
        ]
      }
    },
    {
      "hash": "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec",
      "value": {
        "constr": 0,
        "fields": []
      }
    }
  ]
}
```

### `GET /tx/<txHash>`
Request: `GET /tx/cc4d0a110db98600dafad27b37eea0e1918260bc49081696fdc8b1e01920338e`

Response:
```jsonc
{
  "body":{
    "requiredExtraSignatures":[

    ],
    "withdrawals":{

    },
    "scriptIntegrityHash":null,
    "validityInterval":{
      "invalidHereafter":47480075,
      "invalidBefore":null
    },
    "inputs":[
      {
        "index":1,
        "txId":"de6d46a9d2a511fd33241caabf63390887f5174c779e29174c82414c78f5b49f"
      }
    ],
    "fee":245581,
    "network":null,
    "certificates":[

    ],
    "outputs":[
      {
        "value":{
          "coins":418783107,
          "assets":{
            "689903d80b71e0570fea2fdaaa4bf80989785ed2a2cd57da0d9a7d0a.746f6b656e5f3132":418910,
            "06f8c5655b4e2b5911fee8ef2fc66b4ce64c8835642695c730a3d108.646464":22,
            "06f8c5655b4e2b5911fee8ef2fc66b4ce64c8835642695c730a3d108.736473":4,
            "bfa2e9636c9df09279d21ad14baf84ff4f6c10cc84f61b2388db3e81.74657374746f6b656e":200000,
            "5fe03ede2b378777f68f6b4c4ec47692a6e5bc2af80de3a1781d3296.73656c66746f6b656e31":250000,
            "06f8c5655b4e2b5911fee8ef2fc66b4ce64c8835642695c730a3d108.617364":33,
            "7180cf30d4f4db3037bd815f89f0b348a10e31e11f0a40e6993c8189.594f5550":3000,
            "159bc4f577978a7240d03dbf4b28722fbff1fe4ba37435e94acbaa26":2,
            "308177c8d1c7017a7bd4a7971152e7f4cd1f76759febc1fa12613700.73656c66746f6b656e31":150000,
            "c48f707fea6f08af67a8c06c9bea5b3ec847f5901dc08420cd7f8ade.657269632e6574682e31343333363531333131313832393534353030":1,
            "6b248bf1bbfac692610ca7e9873f988dc5e358b9229be8d6363aedd3.4d59546f6b656e":199999999685,
            "6c6e472b55ad49736568153e7b91d67b1b28a66eb80eb39526d3d247.646466":3,
            "c56f63b1522fbf46fefa6c199aebba42a04e9ff99204cc0b9f557abe.5154546f6b656e":12000,
            "2984b98bab844a0302fed0dab5c787db8f75543f09d9499239e15136.74657374746f6b656e":199995,
            "8617488dde7b8b39881e6034dbd30860c7313d65014f203ed3e57a74.616263":1000,
            "52babbdbf74b3661703376e592733d099510b8f0163c98effc3c18d5.616263":12,
            "6e0d4cf285768594d0611ac0bc8e5c213c740296b76e4b78157b5db2.67696674746f6b656e":500000,
            "6b8d07d69639e9413dd637a1a815a7323c69c86abbafb66dbfdb1aa7":2,
            "6b974ea11a8a23d6df7bd1ff0add1f0bbc1fef46185f0aa4884c17e1.534154":700,
            "00220fdd1fa7718c5257c3051830e8d6af280e93942d53b6b2506f16.73656c66746f6b656e31":120,
            "b9bd3fb4511908402fbef848eece773bb44c867c25ac8c08d9ec3313.696e746a":199962,
            "47101e3f5d731ce43eaa81f6b09e119e8792fc59b952d5f6051ace69.74657374746f6b656e":199995,
            "e1ae600b1e0e2dde90ff4749d192524b6f7d6e08ba8b99afceacab2b.73656c6667696674":99966,
            "159bc4f577978a7240d03dbf4b28722fbff1fe4ba37435e94acbaa26.616263":14,
            "7cc0fd78dd44e38a928109fc7baf60a9e3f1ef530c9840e542cfee93.74657374746f6b656e":200000,
            "c7127649494037a76d9e53b5e9b848b347a624983e5c6c7649f49941.545431":10,
            "329728f73683fe04364631c27a7912538c116d802416ca1eaf2d7a96.736174636f696e":4999873715,
            "52babbdbf74b3661703376e592733d099510b8f0163c98effc3c18d5.74657374":3333,
            "8bb9f400ee6ec7c81c5afa2c656945c1ab06785b9751993653441e32.54455354":343430,
            "8617488dde7b8b39881e6034dbd30860c7313d65014f203ed3e57a74.78797a":2000,
            "4c0ba27aaa43124c6205dcc1314cce1f297ad734877c6523ae7ff6aa.74657374746f6b656e":200000,
            "c56f63b1522fbf46fefa6c199aebba42a04e9ff99204cc0b9f557abe.4554546f6b656e":3000,
            "c7127649494037a76d9e53b5e9b848b347a624983e5c6c7649f49941.545432":20,
            "dd60cac16a3647149cfbefd16e1635e700a18ed5473b7103acb04d23.73656c66746f6b656e31":250000,
            "06f8c5655b4e2b5911fee8ef2fc66b4ce64c8835642695c730a3d108.746f6b656e5f3132":178
          }
        },
        "address":"addr_test1qzx9hu8j4ah3auytk0mwcupd69hpc52t0cw39a65ndrah86djs784u92a3m5w475w3w35tyd6v3qumkze80j8a6h5tuqq5xe8y",
        "datum":null
      },
      {
        "value":{
          "coins":1379280,
          "assets":{
            "4ac3863272a4e23d99001375a52e21b3ab9dec68ac390f15f2815b78.73656c66746f6b656e31":250000
          }
        },
        "address":"addr_test1qqwpl7h3g84mhr36wpetk904p7fchx2vst0z696lxk8ujsjyruqwmlsm344gfux3nsj6njyzj3ppvrqtt36cp9xyydzqzumz82",
        "datum":null
      }
    ],
    "collaterals":[

    ],
    "mint":{
      "coins":0,
      "assets":{
        "4ac3863272a4e23d99001375a52e21b3ab9dec68ac390f15f2815b78.73656c66746f6b656e31":250000
      }
    },
    "update":null
  },
  "witness":{
    "scripts":{
      "4ac3863272a4e23d99001375a52e21b3ab9dec68ac390f15f2815b78":{
        "native":{
          "2":[
            "8be4ff3d2e63ee8ded93cb20e25c56c3d3ba37099dee28e6f04a08e8",
            "e2a0b6adf5249b5bfdc5596dab387b32fb6e0e9c25f8dfde7be2026d",
            "f66a20bab2ac4f592300263133cb4ecce3da6e3a2fc622601df885cc"
          ]
        }
      }
    },
    "signatures":{
      "bac7fc4d147f683399be36653ad387de74056d172d4cf5f8a6f0ebae066c121b":"2md38RKEfEQeDBjkTLPEiMZ/tRlmX7LwCZ4/qSU/+r7E3bMWHDEMlhWjR6wDjqjzwqZI5CoQP4zUVnepmAo0AQ==",
      "9518c18103cbdab9c6e60b58ecc3e2eb439fef6519bb22570f391327381900a8":"YsgzSq38E/7ZDHjPeN4tjkNLwVTXS2kZ04kaT211bO4/4CJ8AhIkCZjTgvd+QaD1iqlhuOdXKb+L/yuNyS+vBA==",
      "fa822bccea21197229226f2b640ee50db5d60d19407e20ae03855cf231cc97a0":"S/9eTokJ1ZR5iMYsIzcDHw0LTps4WU1O5cVRbr6NqEfAWzofiZnmqmnX7GMCOPG60zDUTVCpzyZzMtQ3b+CmAQ==",
      "0912e8521d6e26f05d02cb9a586014b2f5898b5d042c265a338268b2dffae8de":"qkOjoFflrh6cSxN8IO4iKg+p2YBCv/v9UVOyIolnaKHGFz8aAZBDqXWUCx6jL4dzb4foCIUGWLFcXzyhGjDsBQ=="
    },
    "datums":{

    },
    "bootstrap":[

    ],
    "redeemers":{

    }
  },
  "metadata":{
    "hash":"bdaa99eb158414dea0a91d6c727e2268574b23efe6e08ab3b841abe8059a030c",
    "body":{
      "scripts":[

      ],
      "blob":{

      }
    }
  },
  "id":"cc4d0a110db98600dafad27b37eea0e1918260bc49081696fdc8b1e01920338e"
}
```


### `GET /block`

Returns block that was recently processed.

Response
```jsonc
{
  "blockId": "073f35fab0800201698628ef9e6bc85d05dcc78fc87c1f0633a8c4bd93a804d8",
  "blockSlot": 47189428
}
```

## Control API

### `POST /control/startingBlock`

Request header:

* Basic access authentication: `Authorization: Basic dXNyOnB3ZA==`, where `dXNyOnB3ZA====` is `usr:pwd` string in Base64 encoding.

Request body:
```jsonc
{
  "startingBlock": {
    "blockSlot": 59809992,
    "blockId": "7c8aec019a21ffd0049d64b0c9874d93376ed5662b4cf7d78e186b5958ecb00d"
  }
}
```

Responses:
* 200 `{"hash":"7c8aec019a21ffd0049d64b0c9874d93376ed5662b4cf7d78e186b5958ecb00d","slot":59809992}`
* 401 Unauthorized
* 403 Forbidden

### `POST /control/datumFilter`

Request header:

* Basic access authentication: `Authorization: Basic dXNyOnB3ZA==`, where `dXNyOnB3ZA====` is `usr:pwd` string in Base64 encoding.

Request body:
```jsonc
{
  "datumFilter": {"all": []}
}
```

Responses:
* 200 `[]`
* 401 Unauthorized
* 403 Forbidden


### `GET /healthcheck`
Response:
* 200 `[]`

## WebSocket API methods

Endpoint: `/ws`

### Datum query

#### GetDatumByHash

Request:
```jsonc
{
  "type": "jsonwsp/request",
  "version": "1.0",
  "servicename": "ogmios-datum-cache",
  "methodname": "GetDatumByHash",
  "args": {
    "hash": "04caaf1336b754e0b8b4e2fa1c59aa6b85f97dd29652729f1c1e28805acdeb20"
  },
  // optional
  "mirror": {"meta": "this object will be mirrored under 'reflection' field in a response to this request"}
}
```

Response (datum found):
```
{
  "methodname": "GetDatumByHash",
  "result": { DatumFound: { value: [Object] } },
  "version": "1.0",
  "servicename": "ogmios-datum-cache",
  "type": "jsonwsp/response",
  "reflection": {"meta": "this object will be mirrored under 'reflection' field in a response to this request"}
}
```

```jsonc
{
  "methodname": "GetDatumByHash",
  "result": {
    "DatumFound": {
      "value": {
        "constr": 3,
        "fields": [
          {
            "constr": 0,
            "fields": [
              {
                "constr": 0,
                "fields": [
                  "",
                  ""
                ]
              },
              {
                "constr": 0,
                "fields": [
                  "648823ffdad1610b4162f4dbc87bd47f6f9cf45d772ddef661eff198",
                  "76455448"
                ]
              }
            ]
          },
          100000
        ]
      }
    }
  },
  "version": "1.0",
  "servicename": "ogmios-datum-cache",
  "type": "jsonwsp/response",
  "reflection": {"meta": "this object will be mirrored under 'reflection' field in a response to this request"}
}

```

Response (datum not found):
```
{
  "methodname": "GetDatumByHash",
  "result": { DatumNotFound: null },
  "version": "1.0",
  "servicename": "ogmios-datum-cache",
  "type": "jsonwsp/response",
  "reflection": {"meta": "this object will be mirrored under 'reflection field in a response to this request"}
}
```

Response (fault):
```
{
  "methodname": "GetDatumByHash",
  "version": "1.0",
  "fault": { string: "Error deserializing plutus Data", code: "client" },
  "servicename": "ogmios-datum-cache",
  "type": "jsonwsp/fault",
  "reflection": {"meta": "this object will be mirrored under 'reflection' field in a response to this request"}
}
```

#### GetDatumsByHashes

Datums missing in the db are omitted from the response, if none datums are found an empty array is returned.

Request:
```jsonc
{
  "type": "jsonwsp/request",
  "version": "1.0",
  "servicename": "ogmios-datum-cache",
  "methodname": "GetDatumsByHashes",
  "args": {
    "hashes": [
      "abc",
      "04caaf1336b754e0b8b4e2fa1c59aa6b85f97dd29652729f1c1e28805acdeb20"
    ]
  },
  "mirror": "req.no.1"
}
```

Response:
```
{
  "methodname": "GetDatumsByHashes",
  "result": { value: [Array] },
  "version": "1.0",
  "servicename": "ogmios-datum-cache",
  "type": "jsonwsp/response",
  "reflection": "req.no.1"
}
```

```jsonc
{
  "methodname": "GetDatumsByHashes",
  "result": {
      "value": [
        {
          "hash": "04caaf1336b754e0b8b4e2fa1c59aa6b85f97dd29652729f1c1e28805acdeb20",
          "value": {
            "Right":{ 
              "constr": 3,
              "fields": [
              {
                  "constr": 0,
                  "fields": [
                    {
                      "constr": 0,
                      "fields": [
                        "",
                        ""
                      ]
                    },
                    {
                      "constr": 0,
                      "fields": [
                        "648823ffdad1610b4162f4dbc87bd47f6f9cf45d772ddef661eff198",
                        "76455448"
                      ]
                    }
                  ]
                },
                100000
              ]
            }
          }
        },
        {
        "hash" : "3ffda3ffdad1610b4162f4dbc87bd47f6f9cf45d772ddef661eff198",
        "value": {
          "Left": {
              "error" : "ErrorReason"
            }
          }
        }
      ]
  },
  "version": "1.0",
  "servicename": "ogmios-datum-cache",
  "type": "jsonwsp/response",
  "reflection": "req.no.1"
}
```

Response (fault)
```
{
  "methodname": "GetDatumsByHashes",
  "version": "1.0",
  "fault": {
    "string": "Error deserializing plutus Data in: ["abc"]",
    "code": "client"
  },
  "servicename": :ogmios-datum-cache",
  "type": "jsonwsp/fault",
  "reflection": "req.no.1"
}
```

#### GetTxByHash
Request:
```jsonc
{
  "type": "jsonwsp/request",
  "version": "1.0",
  "servicename": "ogmios-datum-cache",
  "methodname": "GetTxByHash",
  "args": {
    "hash": "cc4d0a110db98600dafad27b37eea0e1918260bc49081696fdc8b1e01920338e"
  }
}
```

Response (tx found):
```jsonc
{
  "reflection":null,
  "methodname":"GetTxByHash",
  "result":{
    "TxFound":{
      "value":{
        "rawTx":"bdaa99eb158414dea0a91d6c727e2268574b23efe6e08ab3b841abe8059a030c",
        "txId":"cc4d0a110db98600dafad27b37eea0e1918260bc49081696fdc8b1e01920338e"
      }
    },
  },
  "version":"1.0",
  "servicename":"ogmios-datum-cache",
  "type":"jsonwsp/response"
}
```

Response (tx not found):
```jsonc
{
  "reflection":null,
  "methodname":"GetTxByHash",
  "result":{
    "TxNotFound":null
  },
  "version":"1.0",
  "servicename":"ogmios-datum-cache",
  "type":"jsonwsp/response"
}
```


#### GetBlock

Request:
```jsonc
{
  "type": "jsonwsp/request",
  "version": "1.0",
  "servicename": "ogmios-datum-cache",
  "methodname": "GetBlock",
  "mirror": "req.no.1"
}
```

Response:
```jsonc
{
  "methodname":"GetBlock",
  "result":{
    "block":{
      "blockId":"a3a4b401629e2f72fc754abf1554f3ca12616581c41450fdb5d15f51daf6c8db",
      "blockSlot":51779970
    }
  },
  "version":"1.0",
  "servicename":"ogmios-datum-cache",
  "type":"jsonwsp/response",
  "reflection": "req.no.1"
}
```

### Control API

#### SetStartingBlock

Request:
```jsonc
{
  "type": "jsonwsp/request",
  "version": "1.0",
  "servicename": "ogmios-datum-cache",
  "methodname": "SetStartingBlock",
  "args": {
    "startingBlock": {
      "blockSlot": 59809992,
      "blockId": "7c8aec019a21ffd0049d64b0c9874d93376ed5662b4cf7d78e186b5958ecb00d"
    },
    "token": "usr:pwd"
  },
  "mirror": "foo"
}
```

Response:
```jsonc
{
  "methodname": "SetStartingBlock",
  "result": {
    "hash":"7c8aec019a21ffd0049d64b0c9874d93376ed5662b4cf7d78e186b5958ecb00d",
    "slot":59809992
  },
  "version": "1.0",
  "servicename": "ogmios-datum-cache",
  "type": "jsonwsp/response",
  "reflection": "foo"
}
```

Response (fault - 1):
```jsonc
{
  "methodname": "SetStartingBlock",
  "version": "1.0",
  "fault": {
    "string": "notFound",
    "code": "client"
  },
  "servicename": "ogmios-datum-cache",
  "type": "jsonwsp/fault",
  "reflection": "foo"
}
```

Response (fault - 2):
```jsonc
{
  "methodname": "SetStartingBlock",
  "version": "1.0",
  "fault": {
    "string": "Control API token not granted",
    "code": "client"
  },
  "servicename": "ogmios-datum-cache",
  "type": "jsonwsp/fault",
  "reflection": "foo"
}
```

#### SetDatumFilter

Request:
```jsonc
{
  "type": "jsonwsp/request",
  "version": "1.0",
  "servicename": "ogmios-datum-cache",
  "methodname": "SetDatumFilter",
  "args": {
    "datumFilter": 
      {
        "all": [
          {"hash": "foobar"}, 
          {"any": [
            {"address": "addr_abc"},
            {"address": "addr_xyz"}
            ]
          }
        ]
      },
    "token": "usr:pwd",
  },
  "reflection": "foo"
}
```

Response:
```jsonc
{
  "methodname": "SetDatumFilter",
  "result": {
  },
  "version": "1.0",
  "servicename": "ogmios-datum-cache",
  "type": "jsonwsp/response",
  "reflection": "foo"
}
```

Response (fault - 1):
```jsonc
{
  "methodname": "SetDatumFilter",
  "version": "1.0",
  "fault": {
    "string": "Control API token not granted",
    "code": "client"
  },
  "servicename": "ogmios-datum-cache",
  "type": "jsonwsp/fault",
  "reflection": "foo"
}
```

#### GetHealthcheck

Request:
```jsonc
{
  "type": "jsonwsp/request",
  "version": "1.0",
  "servicename": "ogmios-datum-cache",
  "methodname": "GetHealthcheck",
  // optional
  "mirror": "foo"
}
```
Response:
```jsonc
{
  "methodname":"GetHealthcheck",
  "result":{ },
  "version":"1.0",
  "servicename":"ogmios-datum-cache",
  "type":"jsonwsp/response",
  "reflection":"foo"
}
```

## Block data from ogmios local chain sync

Structure:
```jsonc
{
  "body": [<transaction>],
  "header": <blockHeader>
}
```

Example of an Alonzo block returned during local chain sync:
```jsonc
{
  "body": [
    <...>
    {
      "witness": {
        "signatures": {
          "367912b30df32ec731cb2bd8e725dbb00d84125cce333fbde55ed95d602fa21d": "8w59f59UTB4g+v36DS3S8YtYG5AOx4682Ultazqng2t+No5NV+HSKSshg7bd5MrFuGzYecK+dzmswsc/SQElCA=="
        },
        "scripts": {},
        "datums": {
          "839d5029e89cf231333478cdef5171d5bcfa81f7c5cc26f9c324ce2aa5ce92ec": "2HmfQWbYeZ/YeZ/YeZ/YeZ9YHNc1tMCTMu73PgOnlHHjOsm3VUFM/zFhTfP+shj/2Hmf2Hmf2HmfWBwIxhwAwzNuwQw81INRAoRu+0Xb0dp0R3GTvWFq/////9h6gP/YeoD/GgAmJaDYep8aAAhG0P//"
        },
        "redeemers": {},
        "bootstrap": []
      },
      "id": "870ccbda260e30cc7a54b36f596e801a006d5571a26b5463b0bdc6d358ab44d9",
      "body": {
        "inputs": [
          {
            "txId": "699462c5b09d17f9e6896600e63276fd4d87ae8948c17d19a53582361302180f",
            "index": 1
          }
        ],
        "collaterals": [],
        "outputs": [
          {
            "address": "addr_test1wrsexavz37208qda7mwwu4k7hcpg26cz0ce86f5e9kul3hqzlh22t",
            "value": {
              "coins": 4500000,
              "assets": {
                "d311d3488cc4fef19d05634adce8534977a3bc6fc18136ad65df1d4f.6c712066": 542416
              }
            },
            "datum": "839d5029e89cf231333478cdef5171d5bcfa81f7c5cc26f9c324ce2aa5ce92ec"
          },
          {
            "address": "addr_test1qrtntdxqjvewaae7qwnegu0r8tymw42pfnlnzc2d70ltyxqgccwqpsendmqsc0x5sdgs9prwldzah5w6w3rhryaav94qk9yjlg",
            "value": {
              "coins": 478832529,
              "assets": {
                "126b8676446c84a5cd6e3259223b16a2314c5676b88ae1c1f8579a8f.7441444158": 51625,
                "436a021d1c4f2ea7408f00b97cf67502d9707ae45dbe1cd8ac0f74cb.56414e53": 7157,
                "57fca08abbaddee36da742a839f7d83a7e1d2419f1507fcbf3916522.43484f43": 5322320,
                "57fca08abbaddee36da742a839f7d83a7e1d2419f1507fcbf3916522.524245525259": 473212646,
                "57fca08abbaddee36da742a839f7d83a7e1d2419f1507fcbf3916522.534245525259": 541969927,
                "57fca08abbaddee36da742a839f7d83a7e1d2419f1507fcbf3916522.56414e494c": 12732105,
                "80adfdbdeec01d43cacff17056221c08e67d73e51319d18af0f55520.534d494c4b": 102,
                "b07de2ce2a86f890410d4504d491b1df423f7e3e20973663a819d1a1.53554e444145": 8459833,
                "d311d3488cc4fef19d05634adce8534977a3bc6fc18136ad65df1d4f.6c712025": 431196429,
                "d311d3488cc4fef19d05634adce8534977a3bc6fc18136ad65df1d4f.6c7120fb": 13789128
              }
            },
            "datum": null
          }
        ],
        "certificates": [],
        "withdrawals": {},
        "fee": 194013,
        "validityInterval": {
          "invalidBefore": null,
          "invalidHereafter": null
        },
        "update": null,
        "mint": {
          "coins": 0,
          "assets": {}
        },
        "network": null,
        "scriptIntegrityHash": "e39d3efd01c316d4722a93c3a584bc1650050f012edaf6175b2280c177596e06",
        "requiredExtraSignatures": []
      },
      "metadata": null
    }
  ],
  "header": {
    "signature": "OF3Z63bBn7Mpi0r1s7rDBruKX1p3A8TxUZBJrdIT8LJixKlaq32QnEfuF0XqE2votMwQ0EwCuSL6+x22wCmTCjJvC8fFdaddvMoziWhpmkRBO1BQxqP/49oIuRJbrtPob+BbdfLq+aAMH1Fgf83M1WQSH/s2mrf8fTidOkxbvF7KT5UVVdMyZxjy9hVKjzSoNOmwLF8e08j/zA+H4/NG5FLCiJXH9KhJZt472c0hs/JeDxVLbEo9uUD6vt0RTuMJ31CyRpWmhXRBLKkWJcX+CybuBySMo5ueJzTiHUFKG9Dubi231PZ5PaUau+oNl388r0+Q5WxfRYFGanJZngUXSziFNfqIZWL5q847RzBmLhEQhOHcMp2STQSXgWPCQW8tMAPudiMIn2fc9Sv5sNZvy0nnhrfnaBuqebYVMfZuglKvaITDeuXAb40aLAg755QuhC18oPIWUQLd4Zgo5M1g2MH1u/Dk8d734R2MkyiHoqpNB/4N00fcqimVx35YQJCjdNaFq4qyfpfrLUBWX3/kH8/hziWezCf6pZynO0gBm7SkhOUgtzKzLqjdUwI/8g8jyUzlBnj5Yr2wB4IBOYRiYQ==",
    "nonce": {
      "output": "FHCG213VFPlwwaLIyrE2y03QxzunoQW5IGzk6eD3BgFJ9jkYR0h7RX/EvY1PSgAvgVqL6JJKcbBoaSezq7OqHQ==",
      "proof": "6zp+wopYitV6Wrm1/WqV1BikaCz9Xqk8B3K2jwh/Ki6I0/N4wYnwuQAqaJ1rfFWtUwBg/A08mULFO9Ta1GNzukWtRI82cr5QZJFx/kLFGA0="
    },
    "leaderValue": {
      "output": "AAfbP7QCzgC4Pkfaw5RNNGpBK9AjOnK3Rd+4wn0SCleG4549ASJ7VDjg1Qab6rjALiE8jDiv4RNJIY2vpCCtbQ==",
      "proof": "wDpl6M6kXujpBxmElSMeYN2WjtdNnllNDIail6I0MIiPNY7/1Ma/tpSha90gAH3EqgjI7svM3uzY2MnQzgZKVuP/TxNhocZYgG71onNs1gk="
    },
    "opCert": {
      "hotVk": "+k0Tm8zrtXW5G8TEyieIPgRO49CgpvQKT4Mm19kkxrg=",
      "count": 2,
      "kesPeriod": 297,
      "sigma": "myw5gGHUBKLZePyJXftBHGllCTuHg0nh/19AafMNSOs/OQNUPJXDzudFc80DrlTHeJOe4noU1g3b9cPKuxdbDA=="
    },
    "protocolVersion": {
      "major": 6,
      "minor": 0
    },
    "blockHeight": 3174391,
    "slot": 45819111,
    "prevHash": "1ca566862aa483c71373e33e174e14f12d776fd04b74251db4642649c4a9a327",
    "issuerVk": "55f758252bb6a9dcdd4b3b1e9bdb063102086f56914555e87ac3c0d489c1316c",
    "issuerVrf": "VdhD8XDqfTna1dSibaEJHTsgCWBGBivYY5YEBQ9zlI0=",
    "blockSize": 44893,
    "blockHash": "7a35d4ee2391cdd01a567063ef9df999c3449f9e56e2bbf213e6f9ac143f2c18"
  },
  "headerHash": "aa03bbdd33659be6ece4d73acff51fd875b3dcc4e8c19a58ba94ca7e7b52ec3b"
}
```

## Deployment v1
### Step 1
Run local ogmios instance:
```
docker-compose -f deploy/docker-compose.yml up -d
```

### Step 2 - Passing arguments to ODC

```
Usage: ogmios-datum-cache (--db-port PORT --db-host HOST_NAME
                            --db-user USER_NAME [--db-password PASSWORD]
                            --db-name DB_NAME |
                            --db-connection POSTGRES_LIBPQ_CONNECTION_STRING)
                          --server-port PORT
                          --server-api SERVER_CONTROL_API_TOKEN
                          --ogmios-address ADDRESS --ogmios-port PORT 
                          [--block-slot INT --block-hash HASH | --from-origin | 
                            --from-tip] [--block-filter FILTER] [--use-latest] 
                          [--queue-size NATURAL] [--log-level LOG_LEVEL]

Available options:
  --db-port PORT           Postgres libpq connection port
  --db-host HOST_NAME      Postgres libpq connection host
  --db-user USER_NAME      Postgres libpq connection user
  --db-password PASSWORD   Postgres libpq connection password
  --db-name DB_NAME        Postgres libpq connection data base name
  --db-connection POSTGRES_LIBPQ_CONNECTION_STRING
                           "host=localhost port=5432 user=<user>
                           password=<pass>"
  --server-port PORT       ODC server port
  --server-api SERVER_CONTROL_API_TOKEN
                           Defines the secrete token, required for control API
                           call. Format: user:password
  --ogmios-address ADDRESS Ogmios address
  --ogmios-port PORT       Ogmios port
  --block-slot INT         Slot of first block to fetch by initial block
                           fetcher.
  --block-hash HASH        hash of block's HEADER not hash of a block itself
  --from-origin            Start block fetcher from origin
  --from-tip               Start block fetcher from chain tip
  --block-filter FILTER    Filter.
  --use-latest             defines if block fetcher, if started automatically,
                           should start from last block that was proccessed
                           rather than from block defined with --block-slot and
                           --block-hash.
  --queue-size NATURAL     Defines size of queue of prefetched blocks ready to
                           be processed, default=64.
  --log-level LOG_LEVEL    One of [debug | info | warn | error], every level is
                           more restrictive than the previous level. By default
                           set to info
  --old-ogmios             required if ogmios < 5.5.0
  -h,--help                Show this help text
```

Example: `ogmios-datum-cache --db-connection 'host=localhost port=5432 user=aske dbname=ogmios-datum-cache' --server-port 9999 --server-api 'usr:pwd' --ogmios-address '127.0.0.1' --ogmios-port 1337 --block-slot 44366242 --block-hash d2a4249fe3d0607535daa26caf12a38da2233586bc51e79ed0b3a36170471bf5  --use-latest --block-filter '{"all": [{"hash": "foobar"}, {"any": [{ "address": "addr_abc" },{ "address": "addr_xyz" }]}]}'`

### Datum filter

Datum filter can filter datum hash and address of utxo with given datum. Filters can be combined with logical `or`s and `and`s.

Example (filter will save datums only if hash is `foobar` and (utxo with datum is on address `addr_abc` or `addr_xyz`)):

```jsonc
{
    "all": [
        {
            "hash": "foobar"
        },
        {
            "any": [
                { "address": "addr_abc" },
                { "address": "addr_xyz" }
            ]
        }
    ]
}

```

### Available support channels info

You can find help, more information and ongoing discussion about the project here:
- [#ogmios-datum-cache](https://mlabs-corp.slack.com/archives/C03HVDBU9FT) - mlabs ODC slack channel.
