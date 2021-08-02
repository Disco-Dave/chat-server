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
import Html as H
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Decode
import RoomConnection
import SharedViews exposing (layout, textField)
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
                    , Cmd.none
                    )

                RoomConnection.UserJoinedMessage user ->
                    ( { model | receivedEvents = UserJoined user :: model.receivedEvents }
                    , Nothing
                    , Cmd.none
                    )

                RoomConnection.SentMessage time user text ->
                    ( { model | receivedEvents = MessageReceived time user text :: model.receivedEvents }
                    , Nothing
                    , Cmd.none
                    )

        ReceivedRoomEvent (Err _) ->
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


viewEvent : Zone -> ReceivedEvent -> H.Html msg
viewEvent localZone event =
    case event of
        UserJoined user ->
            H.p [ A.class "user-event user-event--joined" ]
                [ H.span [ A.class "user-event__name" ] [ H.text user.name ]
                ]

        UserLeft user ->
            H.p [ A.class "user-event user-event--left" ]
                [ H.span [ A.class "user-event__name" ] [ H.text user.name ]
                ]

        MessageReceived timestamp user message ->
            H.article [ A.class "message" ]
                [ H.header [ A.class "message__header" ]
                    [ H.span [ A.class "message__user" ] [ H.text user.name ]
                    , H.span [ A.class "message__timestamp" ] [ H.text (displayTimestamp localZone timestamp) ]
                    ]
                , H.p [ A.class "message__body" ] [ H.text message ]
                ]


view : Model -> Document Msg
view model =
    layout model.room
        [ H.form [ A.class "form", E.onSubmit MessageSent ]
            [ H.div [ A.class "messages" ]
                (List.map (viewEvent model.localTimeZone) model.receivedEvents)
            , textField
                { id = "message"
                , label = "Send a message"
                , value = model.message
                , error = Nothing
                , onBlur = MessageBlurred
                , onInput = MessageChanged
                }
            , H.div [ A.class "buttons" ]
                [ H.button
                    [ A.class "button"
                    , A.id "send-message"
                    , A.type_ "submit"
                    ]
                    [ H.text "Send" ]
                , H.button
                    [ A.class "button button--danger"
                    , A.id "leave"
                    , A.type_ "button"
                    , E.onClick LeaveRequested
                    ]
                    [ H.text "Leave" ]
                ]
            ]
        ]
