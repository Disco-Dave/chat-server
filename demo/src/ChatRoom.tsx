import useRoom, { Announcement } from "./useRoom";

export default function ChatRoom(props: Announcement) {
  const room = useRoom(props);

  return (
    <>
      <div className="chat-room">
        <div className="chat-room__messages">
        </div>
        <div className="chat-room__send">
        </div>
      </div>
    </>
  );
}
