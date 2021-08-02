module Main exposing (main)

import Browser
import Html
import Pages.ChatRoom as ChatRoom
import Pages.SelectRoom as SelectRoom


type Model
    = SelectRoomModel SelectRoom.Model
    | ChatRoomModel ChatRoom.Model


type Msg
    = SelectRoomMsg SelectRoom.Msg
    | ChatRoomMsg ChatRoom.Msg


init : ( Model, Cmd Msg )
init =
    ( SelectRoomModel SelectRoom.init, Cmd.none )


subscriptions : Sub Msg
subscriptions =
    Sub.map ChatRoomMsg ChatRoom.subscriptions


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( SelectRoomMsg pageMsg, SelectRoomModel pageModel ) ->
            let
                ( newModel, outMsg ) =
                    SelectRoom.update pageMsg pageModel
            in
            case outMsg of
                Just (SelectRoom.RoomSelected selectedRoom) ->
                    ChatRoom.init selectedRoom
                        |> Tuple.mapBoth ChatRoomModel (Cmd.map ChatRoomMsg)

                Nothing ->
                    ( SelectRoomModel newModel, Cmd.none )

        ( ChatRoomMsg pageMsg, ChatRoomModel pageModel ) ->
            let
                ( newPageModel, outMsg, pageCmd ) =
                    ChatRoom.update pageMsg pageModel

                newModel =
                    case outMsg of
                        Just ChatRoom.GoBack ->
                            SelectRoomModel SelectRoom.init

                        Nothing ->
                            ChatRoomModel newPageModel
            in
            ( newModel, Cmd.map ChatRoomMsg pageCmd )

        _ ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        mapView mapper { title, body } =
            { title = title
            , body = List.map (Html.map mapper) body
            }
    in
    case model of
        SelectRoomModel pageModel ->
            mapView SelectRoomMsg (SelectRoom.view pageModel)

        ChatRoomModel pageModel ->
            mapView ChatRoomMsg (ChatRoom.view pageModel)


main : Program () Model Msg
main =
    Browser.document
        { init = always init
        , view = view
        , update = update
        , subscriptions = always subscriptions
        }
