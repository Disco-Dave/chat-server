module SharedViews exposing
    ( ButtonModel
    , FieldModel
    , ResetButton
    , SubmitButton
    , TextFieldModel
    , button
    , buttons
    , field
    , layout
    , resetButton
    , submitButton
    , textField
    , form
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


type alias ButtonModel_ msg =
    { id : String
    , text : String
    , type_ : String
    , onClick : Maybe msg
    , isDangerious : Bool
    }


button_ : ButtonModel_ msg -> Html msg
button_ model =
    H.button
        ([ A.classList [ ( "button", True ), ( "button--danger", model.isDangerious ) ]
         , A.id model.id
         , A.type_ model.type_
         ]
            ++ (case model.onClick of
                    Nothing ->
                        []

                    Just e ->
                        [ E.onClick e ]
               )
        )
        [ H.text model.text ]


type alias SubmitButton =
    { id : String
    , text : String
    }


submitButton : SubmitButton -> Html msg
submitButton model =
    button_
        { id = model.id
        , text = model.text
        , type_ = "submit"
        , onClick = Nothing
        , isDangerious = False
        }


type alias ResetButton msg =
    { id : String
    , text : String
    , onClick : msg
    }


resetButton : ResetButton msg -> Html msg
resetButton model =
    button_
        { id = model.id
        , text = model.text
        , type_ = "reset"
        , onClick = Just model.onClick
        , isDangerious = True
        }


type alias ButtonModel msg =
    { id : String
    , text : String
    , onClick : msg
    , isDangerous : Bool
    }


button : ButtonModel msg -> Html msg
button model =
    button_
        { id = model.id
        , text = model.text
        , type_ = "button"
        , onClick = Just model.onClick
        , isDangerious = model.isDangerous
        }


buttons : List (Html msg) -> Html msg
buttons children =
    H.div [ A.class "buttons" ] children


form : msg -> List (Html msg) -> Html msg
form onSubmit children =
    H.form [ A.class "form", E.onSubmit onSubmit ] children
