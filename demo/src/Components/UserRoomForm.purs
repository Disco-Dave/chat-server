module Components.UserRoomForm where

import Prelude
import Components.TextFieldInput (textFieldInput)
import Data.Maybe (Maybe(..))
import Data.String as String
import React.Basic.DOM as R
import React.Basic.Hooks ((/\))
import React.Basic.Hooks as Hooks

type TextField
  = { value :: String
    , error :: Maybe String
    }

changeTextField :: String -> TextField -> TextField
changeTextField newValue textField = textField { value = newValue }

blurTextField :: TextField -> TextField
blurTextField textField =
  let
    value = String.trim textField.value

    error =
      if value == "" then
        Just "May not be empty."
      else
        Nothing
  in
    { value, error }

type State
  = { room :: TextField
    , user :: TextField
    }

initialState :: State
initialState =
  { room: { value: "", error: Nothing }
  , user: { value: "", error: Nothing }
  }

changeRoom :: String -> State -> State
changeRoom newValue state = state { room = changeTextField newValue state.room }

blurRoom :: State -> State
blurRoom state = state { room = blurTextField state.room }

changeUser :: String -> State -> State
changeUser newValue state = state { user = changeTextField newValue state.user }

blurUser :: State -> State
blurUser state = state { user = blurTextField state.user }

mkUserRoomForm :: Hooks.Component Unit
mkUserRoomForm =
  Hooks.component "UserRoomForm"
    $ \_ -> Hooks.do
        state /\ setState <- Hooks.useState initialState
        pure
          $ R.form
              { className: "form"
              , children:
                  [ textFieldInput
                      { id: "user-name"
                      , label: "User"
                      , value: state.user.value
                      , error: state.user.error
                      , onBlur: setState blurUser
                      , onChange: setState <<< changeUser
                      }
                  , textFieldInput
                      { id: "room-name"
                      , label: "Room"
                      , value: state.room.value
                      , error: state.room.error
                      , onBlur: setState blurRoom
                      , onChange: setState <<< changeRoom
                      }
                  ]
              }
