module RoomConnection exposing
    ( Announcement
    , Event(..)
    , MessageText
    , User
    , UserId
    , UserName
    , join
    , leave
    , send
    , subscribe
    )

import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Ports
import Time exposing (Posix)


type alias UserId =
    String


type alias UserName =
    String


type alias RoomId =
    String


type alias User =
    { id : UserId
    , name : UserName
    }


decodeUser : Decoder User
decodeUser =
    Decode.map2 User
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)


type alias Announcement =
    { userName : UserName
    , roomId : RoomId
    }


encodeAnnouncement : Announcement -> Value
encodeAnnouncement { userName, roomId } =
    Encode.object
        [ ( "userName", Encode.string userName )
        , ( "roomId", Encode.string roomId )
        ]


type alias MessageText =
    String


type Event
    = Connected UserId
    | Disconnected
    | FailedToConnect
    | UserLeftMessage User
    | UserJoinedMessage User
    | SentMessage Posix User MessageText


decodeEvent : Decoder Event
decodeEvent =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\messageType ->
                case messageType of
                    "CONNECTED" ->
                        Decode.map Connected (Decode.field "userId" Decode.string)

                    "FAILED_TO_CONNECT" ->
                        Decode.succeed FailedToConnect

                    "DISCONNECTED" ->
                        Decode.succeed Disconnected

                    "USER_LEFT" ->
                        Decode.map UserLeftMessage (Decode.field "user" decodeUser)

                    "USER_JOINED" ->
                        Decode.map UserJoinedMessage (Decode.field "user" decodeUser)

                    "SENT_MESSAGE" ->
                        Decode.map3 SentMessage
                            (Decode.field "timestamp" Iso8601.decoder)
                            (Decode.field "user" decodeUser)
                            (Decode.field "message" (Decode.map (String.dropLeft 1 >> String.dropRight 1) Decode.string))

                    _ ->
                        Decode.fail (messageType ++ " is not a recognized type.")
            )


join : Announcement -> Cmd msg
join =
    Ports.connect << encodeAnnouncement


leave : Cmd msg
leave =
    Ports.close ()


send : MessageText -> Cmd msg
send =
    Ports.send << Encode.string


subscribe : Sub (Result Decode.Error Event)
subscribe =
    Ports.receive (Decode.decodeValue decodeEvent)
