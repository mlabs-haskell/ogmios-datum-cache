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
```
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
```
{
  "hashes": [
    "e827cc9fab9038391dabbe6b79440d7a14c4a38de5a69b2e130acbb46b5ae6ed",
    "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"
  ]
}
```

Response
```
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

## Control API
### `POST /control/add_hashes`

### `POST /control/remove_hashes`

### `POST /control/set_hashes`

### `GET /control/get_hashes`

## Block data from ogmios
