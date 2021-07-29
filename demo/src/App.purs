module App (mkApp) where

import Prelude
import React.Basic.DOM as R
import React.Basic.Hooks as React

mkApp :: React.Component Unit
mkApp =
  React.component "App" \_ -> React.do
    pure $ R.h1_ [ R.text "Hello world" ]
