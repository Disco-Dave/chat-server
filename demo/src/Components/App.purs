module Components.App (mkApp) where

import Prelude
import Components.Layout (layout)
import Components.UserRoomForm (mkUserRoomForm)
import React.Basic.Hooks as React

mkApp :: React.Component Unit
mkApp = do
  userRoomForm <- mkUserRoomForm
  React.component "App" \_ -> React.do
    pure
      $ layout
          { title: "Join a Room"
          , children:
              [ userRoomForm { onSubmit: \_ -> pure unit }
              ]
          }
