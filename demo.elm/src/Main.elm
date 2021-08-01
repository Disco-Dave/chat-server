module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.SelectRoom as SelectRoom


type alias Flags =
    ()


type Model
    = SelectRoomModel SelectRoom.Model


type Msg
    = SelectRoomMsg SelectRoom.Msg


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( SelectRoomModel SelectRoom.init, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( SelectRoomMsg pageMsg, SelectRoomModel pageModel ) ->
            let
                ( newModel, _ ) =
                    Tuple.mapFirst SelectRoomModel (SelectRoom.update pageMsg pageModel)
            in
            ( newModel, Cmd.none )


mapDocument : (msg -> Msg) -> Browser.Document msg -> Browser.Document Msg
mapDocument mapper { title, body } =
    { title = title
    , body = List.map (Html.map mapper) body
    }


view : Model -> Browser.Document Msg
view model =
    case model of
        SelectRoomModel pageModel ->
            mapDocument SelectRoomMsg (SelectRoom.view pageModel)


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
