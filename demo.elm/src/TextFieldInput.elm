module TextFieldInput exposing (Model, textFieldInput)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model msg =
    { id : String
    , label : String
    , value : String
    , error : Maybe String
    , onInput : String -> msg
    , onBlur : msg
    }


fieldClass : { m | error : Maybe String } -> List ( String, Bool )
fieldClass model =
    [ ( "field", True )
    , ( "field--invalid"
      , case model.error of
            Just _ ->
                True

            Nothing ->
                False
      )
    ]


feedback : { m | error : Maybe String } -> Html msg
feedback model =
    case model.error of
        Nothing ->
            text ""

        Just error ->
            p [ class "field__feedback" ] [ text error ]


textFieldInput : Model msg -> Html msg
textFieldInput model =
    div [ classList (fieldClass model) ]
        [ label [ class "field__label", for model.id ] [ text model.label ]
        , input
            [ class "field__input"
            , id model.id
            , name model.id
            , type_ "text"
            , value model.value
            , onInput model.onInput
            , onBlur model.onBlur
            ]
            []
        , feedback model
        ]
