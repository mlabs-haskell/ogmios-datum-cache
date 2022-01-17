# ogmios-datum-cache
## Datum query
### Plutus `Data` in JSON
...
### `GET /datum/<hash>`
Response:
```
<datum value in JSON>
```

### `GET /datums`
Request:
```
{"hashes": ["a", "b", "c"]}
```

Response
```
{"datums": [
  {"hash": "a", "value": <datum value>}
  {"hash": "b", "value": <datum value>}
  {"hash": "c", "value": <datum value>}
  ]
}
```

## Control API
### `POST /control/add_hashes`

### `POST /control/remove_hashes`

### `POST /control/set_hashes`

### `GET /control/get_hashes`
