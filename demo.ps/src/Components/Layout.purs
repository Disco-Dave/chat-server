module Components.Layout
  ( LayoutProps
  , layout
  ) where

import React.Basic.DOM as R
import React.Basic.Hooks (JSX)

type LayoutProps
  = { title :: String
    , children :: Array JSX
    }

layout :: LayoutProps -> JSX
layout props =
  R.main
    { className: "layout"
    , children:
        [ R.h1 { className: "layout__title", children: [ R.text props.title ] }
        , R.div { className: "layout__body", children: props.children }
        ]
    }
