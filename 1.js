const WebSocket = require('ws');
const client = new WebSocket('ws://localhost:9999/ws');

client.on('message', function(msg) {
    const response = JSON.parse(msg);
    console.log(response);
    console.log(JSON.stringify(response));
});

client.once('open', () => {
    console.log('opened');
    // const req = JSON.stringify({
    //     type: "jsonwsp/request",
    //     version: "1.0",
    //     servicename: "ogmios-datum-cache",
    //     methodname: "GetDatumsByHashes",
    //     args: { hashes: ["abc", "04caaf1336b754e0b8b4e2fa1c59aa6b85f97dd29652729f1c1e28805acdeb20"] }
    // });
    // const req = JSON.stringify({
    //     type: "jsonwsp/request",
    //     version: "1.0",
    //     servicename: "ogmios-datum-cache",
    //     methodname: "GetDatumByHash",
    //     // args: { hash: "abc" }
    //     args: { hash: "04caaf1336b754e0b8b4e2fa1c59aa6b85f97dd29652729f1c1e28805acdeb20" }
    // });
    // const req = JSON.stringify({
    //     type: "jsonwsp/request",
    //     version: "1.0",
    //     servicename: "ogmios-datum-cache",
    //     methodname: "StartFetchBlocks",
    //     args: { slot: 1, id: "abc" }
    // });
    const req = JSON.stringify({
        type: "jsonwsp/request",
        version: "1.0",
        servicename: "ogmios-datum-cache",
        methodname: "CancelFetchBlocks",
    });
    console.log(req)
    client.send(req);
});
