module Hooks.UseRoom
  ( Announcement
  , User
  , RoomEvent(..)
  ) where

import Prelude
import Control.Alternative ((<|>))
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, getField)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), unformat)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import React.Basic.Hooks (type (/\), (/\))
import React.Basic.Hooks as Hooks

newtype ServerTimestamp
  = ServerTimestamp DateTime

serverTimestampFormat :: Formatter
serverTimestampFormat =
  let
    dash = Placeholder "-"

    colon = Placeholder ":"
  in
    YearFull
      : dash
      : MonthTwoDigits
      : dash
      : DayOfMonthTwoDigits
      : Placeholder "T"
      : Hours24
      : colon
      : MinutesTwoDigits
      : colon
      : SecondsTwoDigits
      : Placeholder "."
      : Milliseconds
      : Placeholder "Z"
      : Nil

instance decodeServerTimestamp :: DecodeJson ServerTimestamp where
  decodeJson json = do
    unparsed <- decodeJson json
    case unformat serverTimestampFormat unparsed of
      Right dateTime -> Right $ ServerTimestamp dateTime
      Left err -> Left $ TypeMismatch err

type Announcement
  = { userName :: String
    , roomId :: String
    }

type User
  = { id :: String
    , name :: String
    }

data RoomEvent
  = UserLeftRoomEvent User
  | UserJoinedRoomEvent User
  | SentMessageRoomEvent DateTime User String

derive instance eqRoomEvent :: Eq RoomEvent

instance decodeRoomEvent :: DecodeJson RoomEvent where
  decodeJson json = do
    obj <- decodeJson json
    tag <- getField obj "type"
    case tag of
      "USER_LEFT" -> UserLeftRoomEvent <$> decodeJson json
      "USER_JOINED" -> UserJoinedRoomEvent <$> decodeJson json
      "SENT_MESSAGE" -> do
        user <- getField obj "user"
        (ServerTimestamp timestamp) <- getField obj "timestamp"
        message <- getField obj "message"
        Right $ SentMessageRoomEvent timestamp user message
      _ -> Left $ TypeMismatch (tag <> " is not recognized.")

data SocketMessage
  = UserIdReceivedMessage String
  | RoomMessage RoomEvent

instance decodeSocketMessage :: DecodeJson SocketMessage where
  decodeJson json = (RoomMessage <$> decodeJson json) <|> (UserIdReceivedMessage <$> decodeJson json)

type State
  = { userId :: Maybe String
    , events :: Array RoomEvent
    }

initialState :: State
initialState = { userId: Nothing, events: [] }

useRoom :: Announcement -> 
useRoom announcement = do
  state /\ setState <- Hooks.useState { userId: Nothing, events: [] }
  pure unit
