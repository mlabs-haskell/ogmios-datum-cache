const WebSocket = require('ws');
const client = new WebSocket('ws://localhost:9999/ws');

client.on('message', function(msg) {
    const response = JSON.parse(msg);
    console.log(response);
});

client.once('open', () => {
    console.log('opened');
    const req = JSON.stringify({
        type: "jsonwsp/request",
        version: "1.0",
        servicename: "ogmios",
        methodname: "GetDatumsByHashes",
        args: { hashes: ["abc"] }
    });
    console.log(req)
    client.send(req);
});
