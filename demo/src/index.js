import {Elm} from './Main.elm';

const webSocketAddress = process.env.CHAT_SERVER_WEB_SOCKETS_ADDRESS;

const app = Elm.Main.init();

let connection = null;

function close() {
  if (connection && connection.close) {
    connection.close();
    connection = null;
  }
}
app.ports?.close.subscribe(close);

function getNewConnection() {
  try {
    console.log(webSocketAddress)
    return new WebSocket(webSocketAddress)
  }
  catch(e) {
    console.error(e);
    app.ports?.receive.send({
      type: 'FAILED_TO_CONNECT'
    });
  }
}

app.ports?.connect.subscribe((message) => {
  close();
  connection = getNewConnection();

  connection.addEventListener('close', () => {
    app.ports?.receive.send({
      type: 'DISCONNECTED'
    });
  });

  connection.addEventListener('message', (event) => {
    const payload = JSON.parse(event?.data);

    if (typeof payload === 'string') {
      app.ports?.receive.send({
        type: 'CONNECTED',
        userId: payload,
      });
    }
    else {
      app.ports?.receive.send(payload);
    }
  });

  connection.addEventListener('open', () => {
    connection.send(JSON.stringify(message));
  });
});

app.ports?.send.subscribe((message) => {
  if (connection && connection.send) {
    connection.send(JSON.stringify(message));
  }
});
