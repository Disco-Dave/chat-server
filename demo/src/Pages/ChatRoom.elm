module Pages.ChatRoom exposing
    ( Model
    , Msg
    , OutMsg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Browser exposing (Document)
import Browser.Dom as Dom
import Html as H exposing (Html)
import Html.Attributes as A
import Json.Decode as Decode
import RoomConnection
import SharedViews as S
import Task
import Time exposing (Posix, Zone)


type ReceivedEvent
    = UserJoined RoomConnection.User
    | UserLeft RoomConnection.User
    | MessageReceived Posix RoomConnection.User RoomConnection.MessageText


type ConnectionStatus
    = PendingConnection
    | Connected
    | FailedToConnect
    | Disconnected


type alias Model =
    { user : String
    , room : String
    , userId : Maybe String
    , receivedEvents : List ReceivedEvent
    , connectionStatus : ConnectionStatus
    , localTimeZone : Zone
    , message : String
    }


type Msg
    = ReceivedRoomEvent (Result Decode.Error RoomConnection.Event)
    | MessageSent
    | MessageChanged String
    | MessageBlurred
    | LeaveRequested
    | GotLocalTimeZone Zone
    | ScrolledToBottom


type OutMsg
    = GoBack


init : { user : String, room : String } -> ( Model, Cmd Msg )
init { user, room } =
    ( { user = user
      , room = room
      , userId = Nothing
      , receivedEvents = []
      , connectionStatus = PendingConnection
      , localTimeZone = Time.utc
      , message = ""
      }
    , Cmd.batch
        [ RoomConnection.join { userName = user, roomId = room }
        , Task.perform GotLocalTimeZone Time.here
        ]
    )


subscriptions : Sub Msg
subscriptions =
    Sub.map ReceivedRoomEvent RoomConnection.subscribe


scrollToBottomOfRoom : Cmd Msg
scrollToBottomOfRoom =
    Dom.getViewportOf "room"
        |> Task.andThen (\info -> Dom.setViewportOf "room" 0 info.scene.height)
        |> Task.attempt (\_ -> ScrolledToBottom)


update : Msg -> Model -> ( Model, Maybe OutMsg, Cmd Msg )
update msg model =
    case msg of
        GotLocalTimeZone zone ->
            ( { model | localTimeZone = zone }
            , Nothing
            , Cmd.none
            )

        MessageSent ->
            if String.isEmpty (String.trim model.message) then
                ( model, Nothing, Cmd.none )

            else
                ( { model | message = "" }
                , Nothing
                , RoomConnection.send model.message
                )

        MessageChanged newValue ->
            ( { model | message = newValue }
            , Nothing
            , Cmd.none
            )

        MessageBlurred ->
            ( { model | message = String.trim model.message }
            , Nothing
            , Cmd.none
            )

        LeaveRequested ->
            ( model
            , Just GoBack
            , RoomConnection.leave
            )

        ReceivedRoomEvent (Ok event) ->
            case event of
                RoomConnection.Connected userId ->
                    ( { model | userId = Just userId, connectionStatus = Connected }
                    , Nothing
                    , Cmd.none
                    )

                RoomConnection.Disconnected ->
                    ( { model | connectionStatus = Disconnected }
                    , Nothing
                    , Cmd.none
                    )

                RoomConnection.FailedToConnect ->
                    ( { model | connectionStatus = FailedToConnect }
                    , Nothing
                    , Cmd.none
                    )

                RoomConnection.UserLeftMessage user ->
                    ( { model | receivedEvents = UserLeft user :: model.receivedEvents }
                    , Nothing
                    , scrollToBottomOfRoom
                    )

                RoomConnection.UserJoinedMessage user ->
                    ( { model | receivedEvents = UserJoined user :: model.receivedEvents }
                    , Nothing
                    , scrollToBottomOfRoom
                    )

                RoomConnection.SentMessage time user text ->
                    ( { model | receivedEvents = MessageReceived time user text :: model.receivedEvents }
                    , Nothing
                    , scrollToBottomOfRoom
                    )

        ReceivedRoomEvent (Err _) ->
            ( model, Nothing, Cmd.none )

        ScrolledToBottom ->
            ( model, Nothing, Cmd.none )


displayTimestamp : Zone -> Posix -> String
displayTimestamp localZone timestamp =
    let
        month =
            case Time.toMonth localZone timestamp of
                Time.Jan ->
                    "1"

                Time.Feb ->
                    "2"

                Time.Mar ->
                    "3"

                Time.Apr ->
                    "4"

                Time.May ->
                    "5"

                Time.Jun ->
                    "6"

                Time.Jul ->
                    "7"

                Time.Aug ->
                    "8"

                Time.Sep ->
                    "9"

                Time.Oct ->
                    "10"

                Time.Nov ->
                    "11"

                Time.Dec ->
                    "12"

        day =
            String.fromInt (Time.toDay localZone timestamp)

        year =
            String.fromInt (Time.toYear localZone timestamp)

        hour =
            String.fromInt (Time.toHour localZone timestamp)

        minutes =
            String.fromInt (Time.toMinute localZone timestamp)

        seconds =
            String.fromInt (Time.toSecond localZone timestamp)
    in
    month ++ "/" ++ day ++ "/" ++ year ++ " " ++ hour ++ ":" ++ minutes ++ ":" ++ seconds


viewUserLeft : RoomConnection.User -> Html msg
viewUserLeft user =
    H.div [ A.class "user-event user-event--left" ]
        [ H.text user.name ]


viewUserJoined : RoomConnection.User -> Html msg
viewUserJoined user =
    H.div [ A.class "user-event user-event--joined" ]
        [ H.text user.name ]


viewMessage : Zone -> Posix -> RoomConnection.User -> RoomConnection.MessageText -> Html msg
viewMessage localZone timestamp user message =
    H.article [ A.class "message" ]
        [ H.header [ A.class "message__header" ]
            [ H.span [ A.class "message__user" ] [ H.text user.name ]
            , H.span [ A.class "message__timestamp" ] [ H.text (displayTimestamp localZone timestamp) ]
            ]
        , H.p [ A.class "message__body" ] [ H.text message ]
        ]


viewEvent : Zone -> ReceivedEvent -> Html msg
viewEvent localZone event =
    case event of
        UserJoined user ->
            viewUserJoined user

        UserLeft user ->
            viewUserLeft user

        MessageReceived timestamp user message ->
            viewMessage localZone timestamp user message


viewEvents : Zone -> List ReceivedEvent -> List (Html msg)
viewEvents localZone =
    List.map (viewEvent localZone)


viewStatus : ConnectionStatus -> Html msg
viewStatus status =
    case status of
        PendingConnection ->
            H.p [ A.class "status" ] [ H.text "Connecting..." ]

        Connected ->
            H.p [ A.class "status status--ok" ] [ H.text "Connected" ]

        FailedToConnect ->
            H.p [ A.class "status status--bad" ] [ H.text "Failed to connect" ]

        Disconnected ->
            H.p [ A.class "status" ] [ H.text "Disconnected" ]


view : Model -> Document Msg
view model =
    S.layout model.room
        [ S.form MessageSent
            [ H.div [ A.class "room", A.id "room" ]
                [ viewStatus model.connectionStatus
                , H.div [ A.class "events" ] (viewEvents model.localTimeZone model.receivedEvents)
                ]
            , S.textField
                { id = "message"
                , label = "Send a message"
                , value = model.message
                , error = Nothing
                , onBlur = MessageBlurred
                , onInput = MessageChanged
                }
            , S.buttons
                [ S.submitButton
                    { id = "send-message"
                    , text = "Send"
                    }
                , S.button
                    { id = "leave"
                    , text = "Leave"
                    , onClick = LeaveRequested
                    , isDangerous = True
                    }
                ]
            ]
        ]
