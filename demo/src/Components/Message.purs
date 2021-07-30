module Components.Message
  ( MessageProps
  , mkMessage
  ) where

import Prelude
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Formatter.DateTime (Formatter, FormatterCommand(..))
import Data.Formatter.DateTime as Formatter
import Data.List as List
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Minutes, negateDuration)
import Effect.Now (getTimezoneOffset)
import React.Basic.DOM as R
import React.Basic.Hooks as React

timestampFormatter :: Formatter
timestampFormatter =
  List.fromFoldable
    [ MonthShort
    , Placeholder " "
    , DayOfMonth
    , Placeholder ", "
    , YearFull
    , Placeholder " "
    , Hours12
    , Placeholder ":"
    , MinutesTwoDigits
    , Placeholder ":"
    , SecondsTwoDigits
    , Placeholder " "
    , Meridiem
    ]

formatTimestamp :: Minutes -> DateTime -> String
formatTimestamp offset timestamp =
  let
    adjustedTimestamp = fromMaybe timestamp $ DateTime.adjust (negateDuration offset) timestamp
  in
    Formatter.format timestampFormatter adjustedTimestamp

type MessageProps
  = { userName :: String
    , timestamp :: DateTime
    , message :: String
    }

mkMessage :: React.Component MessageProps
mkMessage = do
  offset <- getTimezoneOffset
  React.component "Message"
    $ \props ->
        let
          header =
            let
              timestamp = formatTimestamp offset props.timestamp
            in
              R.header
                { className: "message__header"
                , children:
                    [ R.span { className: "message__user", children: [ R.text props.userName ] }
                    , R.span { className: "message__timestamp", children: [ R.text timestamp ] }
                    ]
                }

          body = R.p { className: "message__body", children: [ R.text props.message ] }
        in
          pure $ R.article { className: "message", children: [ header, body ] }
