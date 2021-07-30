module Components.TextFieldInput where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events as Events
import React.Basic.Hooks (JSX)

type TextFieldInputProps
  = { id :: String
    , label :: String
    , value :: String
    , onChange :: (String -> Effect Unit)
    , onBlur :: Effect Unit
    , error :: Maybe String
    }

textFieldInput :: TextFieldInputProps -> JSX
textFieldInput props =
  let
    fieldClassName = "field" <> maybe "" (const " field--invalid") props.error

    feedback = case props.error of
      Nothing -> mempty
      Just errorMessage -> R.p { className: "field__feedback", children: [ R.text errorMessage ] }
  in
    R.div
      { className: fieldClassName
      , children:
          [ R.label { className: "field__label", htmlFor: props.id, children: [ R.text props.label ] }
          , R.input
              { className: "field__input"
              , id: props.id
              , name: props.id
              , type: "text"
              , value: props.value
              , onBlur: Events.handler_ props.onBlur
              , onChange: Events.handler targetValue (props.onChange <<< fromMaybe "")
              }
          , feedback
          ]
      }
