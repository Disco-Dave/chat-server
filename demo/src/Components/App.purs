module Components.App (mkApp) where

import Prelude
import Components.Layout (layout)
import Components.UserRoomForm (mkUserRoomForm)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Hooks ((/\))
import React.Basic.Hooks as Hooks

type AnnouncementInfo
  = { room :: String
    , user :: String
    }

mkAnnouncementPage :: Hooks.Component { onSubmit :: AnnouncementInfo -> Effect Unit }
mkAnnouncementPage = do
  userRoomForm <- mkUserRoomForm
  Hooks.component "AnnouncementPage" \props -> Hooks.do
    pure
      $ layout
          { title: "Join a Room"
          , children:
              [ userRoomForm { onSubmit: props.onSubmit }
              ]
          }

type State
  = { announcementInfo :: Maybe AnnouncementInfo
    }

mkApp :: Hooks.Component Unit
mkApp = do
  announcementPage <- mkAnnouncementPage
  Hooks.component "App" \_ -> Hooks.do
    state /\ setState <- Hooks.useState { announcementInfo: Nothing }
    let
      handleSubmit info = setState \oldState -> oldState { announcementInfo = Just info }
    pure $ 
      case state.announcementInfo of
           Nothing -> announcementPage { onSubmit: handleSubmit }
           Just _ -> R.h1 { className: "layout__title", children: [ R.text "coming soon" ]}
