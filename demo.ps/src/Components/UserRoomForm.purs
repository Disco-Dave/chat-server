module Components.UserRoomForm where

import Prelude
import Components.TextFieldInput (textFieldInput)
import Data.Maybe (Maybe(..), isJust)
import Data.String as String
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events as Events
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

hasErrors :: State -> Boolean
hasErrors state = isJust state.room.error || isJust state.user.error

type Props
  = { onSubmit :: { room :: String, user :: String } -> Effect Unit }

mkUserRoomForm :: Hooks.Component Props
mkUserRoomForm =
  Hooks.component "UserRoomForm"
    $ \props -> Hooks.do
        state /\ setState <- Hooks.useState initialState
        let
          handleSubmit =
            Events.handler preventDefault \_ -> do
              let
                newState = blurRoom $ blurUser state
              setState $ const newState
              if hasErrors newState then
                pure unit
              else
                props.onSubmit
                  { room: newState.room.value
                  , user: newState.user.value
                  }
        pure
          $ R.form
              { className: "form"
              , onSubmit: handleSubmit
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
                  , R.div
                      { className: "buttons"
                      , children:
                          [ R.button
                              { className: "button"
                              , id: "join-room"
                              , type: "submit"
                              , children: [ R.text "Join" ]
                              , onClick: handleSubmit
                              }
                          , R.button
                              { className: "button button--danger"
                              , id: "reset-form"
                              , type: "reset"
                              , children: [ R.text "Reset" ]
                              , onClick: Events.handler_ (setState \_ -> initialState)
                              }
                          ]
                      }
                  ]
              }
