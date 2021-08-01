import * as React from "react";
import * as uuid from "uuid";

export type Announcement = {
  room: string;
  user: string;
};

export type Event =
  | { type: "USER_LEFT"; key: string; user: { id: string; name: string } }
  | { type: "USER_JOINED"; key: string; user: { id: string; name: string } }
  | {
      type: "SENT_MESSAGE";
      key: string;
      user: { id: string; name: string };
      message: string;
      timestamp: Date;
    };

function parseEvent(json: string): string | Event {
  const rawEvent = JSON.parse(json);

  switch (rawEvent?.type) {
    case "SENT_MESSAGE":
      return {
        ...rawEvent,
        timestamp: new Date(rawEvent.timestamp),
      };

    default:
      return rawEvent;
  }
}

const webSocketUrl = process.env.CHAT_SERVER_WEBSOCKET_URL ?? "";

export default function useRoom(announcement: Announcement) {
  const [connection, setConnection] = React.useState<WebSocket | null>(null);
  const [userId, setUserId] = React.useState<string | null>(null);
  const [events, setEvents] = React.useState<Event[]>([]);

  React.useEffect(() => {
    const webSocket = new WebSocket(webSocketUrl);

    webSocket.addEventListener("open", () => {
      webSocket.send(
        JSON.stringify({
          userName: announcement.user,
          roomId: announcement.room,
        })
      );
    });

    webSocket.addEventListener("message", (e: { data: string }) => {
      const event: string | Event = parseEvent(e.data);

      if (typeof event === "string") {
        setUserId(event);
      } else {
        setEvents((oldEvents) => [
          ...oldEvents,
          {
            ...event,
            key: uuid.v4(),
          },
        ]);
      }
    });

    setConnection(webSocket);

    return () => {
      setUserId(null);
      setConnection(null);
      webSocket.close();
    };
  }, [announcement.room, announcement.user]);

  function sendMessage(message: string) {
    if (connection) {
      connection.send(JSON.stringify(message));
    }
  }

  return {
    userId,
    sendMessage,
    events,
  };
}
