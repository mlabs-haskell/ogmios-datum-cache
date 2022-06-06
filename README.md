# ogmios-datum-cache
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

### `POST /control/block`
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
* 200 `{"hash":"7c8aec019a21ffd0049d64b0c9874d93376ed5662b4cf7d78e186b5958ecb00d","slot":59809992`
* 404 - if intersection is not found

### `POST /control/datumFilter`
Request body:
```jsonc
{
  "datumFilter": {
    "hash": "foobar"
  }
}
```
Responses:
* 200 `[]`

### `GET /healthcheck`
Response:
* 200 `[]`

## WebSocket API
### Examples from ogmios-datum-cache
#### GetDatumByHash
Request:
```jsonc
{
  "type": "jsonwsp/request",
  "version": "1.0",
  "servicename": "ogmios",
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
  methodname: 'GetDatumByHash',
  result: { DatumFound: { value: [Object] } },
  version: '1.0',
  servicename: 'ogmios-datum-cache',
  type: 'jsonwsp/response',
  reflection: {"meta": "this object will be mirrored under 'reflection' field in a response to this request"}
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
  methodname: 'GetDatumByHash',
  result: { DatumNotFound: null },
  version: '1.0',
  servicename: 'ogmios-datum-cache',
  type: 'jsonwsp/response',
  reflection: {"meta": "this object will be mirrored under 'reflection' field in a response to this request"}
}
```

Response (fault):
```
{
  methodname: 'GetDatumByHash',
  version: '1.0',
  fault: { string: 'Error deserializing plutus Data', code: 'client' },
  servicename: 'ogmios-datum-cache',
  type: 'jsonwsp/fault',
  reflection: {"meta": "this object will be mirrored under 'reflection' field in a response to this request"}
}
```

#### GetDatumsByHashes
Datums missing in the db are omitted from the response, if none datums are found an empty array is returned.

Request:
```jsonc
{
  "type": "jsonwsp/request",
  "version": "1.0",
  "servicename": "ogmios",
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
  methodname: 'GetDatumsByHashes',
  result: { DatumsFound: { value: [Array] } },
  version: '1.0',
  servicename: 'ogmios-datum-cache',
  type: 'jsonwsp/response',
  reflection: "req.no.1"
}
```

```jsonc
{
  "methodname": "GetDatumsByHashes",
  "result": {
    "DatumsFound": {
      "value": [
        {
          "hash": "04caaf1336b754e0b8b4e2fa1c59aa6b85f97dd29652729f1c1e28805acdeb20",
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
      ]
    }
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
  methodname: 'GetDatumsByHashes',
  version: '1.0',
  fault: {
    string: 'Error deserializing plutus Data in: ["abc"]',
    code: 'client'
  },
  servicename: 'ogmios-datum-cache',
  type: 'jsonwsp/fault',
  reflection: "req.no.1"
}
```

#### GetBlock

Request:
```jsonc
{
  "type": "jsonwsp/request",
  "version": "1.0",
  "servicename": "ogmios",
  "methodname": "GetBlock"
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
    }
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

Response (fault):
```
{
  methodname: SetStartingBlock,
  version: '1.0',
  fault: {
    string: 'notFound',
    code: 'client'
  },
  servicename: 'ogmios-datum-cache',
  type: 'jsonwsp/fault',
  reflection: "foo"
}

#### SetDatumFilter
Request:
```jsonc
{
  "type": "jsonwsp/request",
  "version": "1.0",
  "servicename": "ogmios-datum-cache",
  "methodname": "SetDatumFilter",
  "args": {
    "datumFilter": {
      "hash": "foobar"
    }
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

#### GetHealthcheck
Request:
```jsonc
{
  "type": "jsonwsp/request",
  "version": "1.0",
  "servicename": "ogmios",
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
docker-compose up -f deploy/docker-compose.yml -d
```

### Step 2 - config file
Modify `config.toml` in the app working directory (currently `/home/ubuntu/seabug/ogmios-datum-cache`).

* `dbConnectionString` (postgres libpq connection string) — `host=localhost port=5432 user=<user> password=<pass>`

* `blockFetcher.autoStart` defines if initial block fetcher should start automatically.

* `blockFetcher.filter` defines json encoded [filter](#filter) for initial block fetcher. If not defined filter will accept all datums.

* `blockFetcher.firstBlock.slot` slot of first block to fetch by initial block fetcher.

* `blockFetcher.firstBlock.id` hash of block's HEADER not hash of a block itself.

* `blockFetcher.startFromLast` defines if block fetcher, if started automatically, should start from last block that was proccessed rather than from block defined in `firstFetchBlock`.

### Filter

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
