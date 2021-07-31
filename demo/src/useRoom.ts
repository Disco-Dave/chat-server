import * as React from "react";

export type Announcement = {
  room: string;
  user: string;
};

type RawEvent =
  | { type: "USER_LEFT"; user: { id: string; name: string } }
  | { type: "USER_JOINED"; user: { id: string; name: string } }
  | {
      type: "SENT_MESSAGE";
      user: { id: string; name: string };
      message: string;
      timestamp: string;
    };

export type Event =
  | { type: "USER_LEFT"; user: { id: string; name: string } }
  | { type: "USER_JOINED"; user: { id: string; name: string } }
  | {
      type: "SENT_MESSAGE";
      user: { id: string; name: string };
      message: string;
      timestamp: Date;
    };

function parseEvent(rawEvent: RawEvent): Event {
  switch (rawEvent.type) {
    case "SENT_MESSAGE":
      return {
        ...rawEvent,
        timestamp: new Date(rawEvent.timestamp),
      };

    default:
      return rawEvent;
  }
}

export default function useRoom(announcement: Announcement) {
  const [connection, setConnection] = React.useState<WebSocket | null>(null);
  const [userId, setUserId] = React.useState<string | null>(null);
  const [events, setEvents] = React.useState<Event[]>([]);

  React.useEffect(() => {
    const webSocket = new WebSocket("ws://localhost/api/");

    webSocket.onopen = () => {
      webSocket.send(
        JSON.stringify({
          userName: announcement.user,
          roomId: announcement.room,
        })
      );
    };

    webSocket.onmessage = (e: { data: string }) => {
      const event: string | RawEvent = JSON.parse(e.data);

      if (typeof event === "string") {
        setUserId(event);
      } else {
        setEvents((oldEvents) => [...oldEvents, parseEvent(event)]);
      }
    };

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
