module Components.App (mkApp) where

import Prelude
import Components.Message (mkMessage)
import Components.Layout (layout)
import Components.UserRoomForm (mkUserRoomForm)
import Effect.Now (nowDateTime)
import React.Basic.Hooks as React

mkApp :: React.Component Unit
mkApp = do
  now <- nowDateTime
  message <- mkMessage
  userRoomForm <- mkUserRoomForm
  React.component "App" \_ -> React.do
    pure
      $ layout
          { title: "Example title"
          , children:
              [ message { userName: "David", timestamp: now, message: "This is a test message" }
              , userRoomForm unit
              ]
          }
