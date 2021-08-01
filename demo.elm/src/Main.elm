module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)


type alias Flags =
    ()


type alias Model =
    { 
    }


type Msg
    = NoOp


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Chat Server"
    , body =
        []
    }


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
