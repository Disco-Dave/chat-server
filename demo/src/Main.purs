module Main where

import Prelude
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM as R
import React.Basic.Hooks as React
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML as HTML
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window as Window

mkApp :: React.Component Unit
mkApp =
  React.component "App" \_ -> React.do
    pure $ R.h1_ [ R.text "Hello world" ]

getAppElement :: Effect Element
getAppElement = do
  node <- map toNonElementParentNode (Window.document =<< HTML.window)
  maybe (throw "Unable to find #app element") pure =<< getElementById "app" node

main :: Effect Unit
main = do
  element <- getAppElement
  app <- mkApp
  R.render (app unit) element
