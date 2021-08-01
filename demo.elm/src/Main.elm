module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import TextFieldInput exposing (textFieldInput)


type alias Flags =
    ()


type alias Model =
    { user : { value : String, error : Maybe String }
    , room : { value : String, error : Maybe String }
    }


type Msg
    = UserChanged String
    | UserBlurred
    | RoomChanged String
    | RoomBlurred
    | FormReset
    | FormSubmitted


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { user = { value = "", error = Nothing }
      , room = { value = "", error = Nothing }
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


validate : { m | value : String, error : Maybe String } -> { m | value : String, error : Maybe String }
validate model =
    let
        value =
            String.trim model.value

        error =
            if String.isEmpty value then
                Just "May not be empty"

            else
                Nothing
    in
    { model | error = error, value = value }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RoomChanged newValue ->
            let
                oldRoom =
                    model.room

                newRoom =
                    { oldRoom | value = newValue }
            in
            ( { model | room = newRoom }
            , Cmd.none
            )

        RoomBlurred ->
            ( { model | room = validate model.room }
            , Cmd.none
            )

        UserChanged newValue ->
            let
                oldUser =
                    model.user

                newUser =
                    { oldUser | value = newValue }
            in
            ( { model | user = newUser }
            , Cmd.none
            )

        UserBlurred ->
            ( { model | user = validate model.user }
            , Cmd.none
            )

        FormReset ->
            ( { model
                | user = { value = "", error = Nothing }
                , room = { value = "", error = Nothing }
              }
            , Cmd.none
            )

        FormSubmitted ->
            let
                newModel =
                    { model
                        | user = validate model.user
                        , room = validate model.room
                    }
            in
            ( newModel, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Chat Server"
    , body =
        [ main_ [ class "layout" ]
            [ h1 [ class "layout__title" ] [ text "Join a Room" ]
            , div [ class "layout__body" ]
                [ Html.form [ class "form", onSubmit FormSubmitted ]
                    [ textFieldInput
                        { id = "user-name"
                        , label = "User"
                        , value = model.user.value
                        , error = model.user.error
                        , onBlur = UserBlurred
                        , onInput = UserChanged
                        }
                    , textFieldInput
                        { id = "room-name"
                        , label = "Room"
                        , value = model.room.value
                        , error = model.room.error
                        , onBlur = RoomBlurred
                        , onInput = RoomChanged
                        }
                    , div [ class "buttons" ]
                        [ button
                            [ class "button"
                            , id "join-room"
                            , type_ "submit"
                            , onClick FormSubmitted
                            ]
                            [ text "Join" ]
                        , button
                            [ class "button button--danger"
                            , id "reset-room"
                            , type_ "reset"
                            , onClick FormReset
                            ]
                            [ text "Reset" ]
                        ]
                    ]
                ]
            ]
        ]
    }


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
