module Pages.SelectRoom exposing
    ( Model
    , Msg
    , OutMsg(..)
    , init
    , update
    , view
    )

import Browser exposing (Document)
import Html as H
import Html.Attributes as A
import Html.Events as E
import SharedViews exposing (layout, textField)


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


type OutMsg
    = RoomSelected { user : String, room : String }


init : Model
init =
    { user = { value = "", error = Nothing }
    , room = { value = "", error = Nothing }
    }

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


update : Msg -> Model -> ( Model, Maybe OutMsg )
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
            , Nothing
            )

        RoomBlurred ->
            ( { model | room = validate model.room }
            , Nothing
            )

        UserChanged newValue ->
            let
                oldUser =
                    model.user

                newUser =
                    { oldUser | value = newValue }
            in
            ( { model | user = newUser }
            , Nothing
            )

        UserBlurred ->
            ( { model | user = validate model.user }
            , Nothing
            )

        FormReset ->
            ( { model
                | user = { value = "", error = Nothing }
                , room = { value = "", error = Nothing }
              }
            , Nothing
            )

        FormSubmitted ->
            let
                newModel =
                    { model
                        | user = validate model.user
                        , room = validate model.room
                    }

                outMsg =
                    case ( newModel.room.error, newModel.user.error ) of
                        ( Nothing, Nothing ) ->
                            Just <| RoomSelected { user = newModel.user.value, room = newModel.room.value }

                        _ ->
                            Nothing
            in
            ( newModel, outMsg )


view : Model -> Document Msg
view model =
    layout "Join a Room"
        [ H.form [ A.class "form", E.onSubmit FormSubmitted ]
            [ textField
                { id = "user-name"
                , label = "User"
                , value = model.user.value
                , error = model.user.error
                , onBlur = UserBlurred
                , onInput = UserChanged
                }
            , textField
                { id = "room-name"
                , label = "Room"
                , value = model.room.value
                , error = model.room.error
                , onBlur = RoomBlurred
                , onInput = RoomChanged
                }
            , H.div [ A.class "buttons" ]
                [ H.button
                    [ A.class "button"
                    , A.id "join-room"
                    , A.type_ "submit"
                    ]
                    [ H.text "Join" ]
                , H.button
                    [ A.class "button button--danger"
                    , A.id "reset-room"
                    , A.type_ "reset"
                    , E.onClick FormReset
                    ]
                    [ H.text "Reset" ]
                ]
            ]
        ]
