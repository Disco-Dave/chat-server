module SharedViews exposing
    ( FieldModel
    , TextFieldModel
    , field
    , layout
    , textField
    )

import Browser exposing (Document)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E


layout : String -> List (Html msg) -> Document msg
layout title children =
    { title = "Chat Server - " ++ title
    , body =
        [ H.main_ [ A.class "layout" ]
            [ H.h1 [ A.class "layout__title" ] [ H.text title ]
            , H.div [ A.class "layout__body" ] children
            ]
        ]
    }


type alias FieldModel =
    { id : String
    , label : String
    , error : Maybe String
    }


field : FieldModel -> Html msg -> Html msg
field model control =
    let
        fieldClassList =
            [ ( "field", True )
            , ( "field--invalid"
              , case model.error of
                    Just _ ->
                        True

                    Nothing ->
                        False
              )
            ]

        feedback =
            case model.error of
                Nothing ->
                    H.text ""

                Just error ->
                    H.p [ A.class "field__feedback" ] [ H.text error ]
    in
    H.div [ A.classList fieldClassList ]
        [ H.label [ A.class "field__label", A.for model.id ] [ H.text model.label ]
        , control
        , feedback
        ]


type alias TextFieldModel msg =
    { id : String
    , label : String
    , error : Maybe String
    , value : String
    , onInput : String -> msg
    , onBlur : msg
    }


textField : TextFieldModel msg -> Html msg
textField model =
    field { id = model.id, label = model.label, error = model.error } <|
        H.input
            [ A.class "field__input"
            , A.id model.id
            , A.name model.id
            , A.type_ "text"
            , A.value model.value
            , E.onInput model.onInput
            , E.onBlur model.onBlur
            ]
            []

