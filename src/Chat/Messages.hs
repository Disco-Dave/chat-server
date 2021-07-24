module Chat.Messages (
  UserId (..),
  RoomId (..),
  UserName (..),
  User (..),
  Announcement (..),
  MessageText (..),
  OutputMessage (..),
) where

import Chat.NonEmptyText (NonEmptyText)
import qualified Chat.NonEmptyText as NonEmptyText
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)

newtype UserId = UserId {fromUserId :: UUID}
  deriving (Show, Eq, Ord)

instance Aeson.FromJSON UserId where
  parseJSON = fmap UserId . Aeson.parseJSON

instance Aeson.ToJSON UserId where
  toJSON = Aeson.toJSON . fromUserId
  toEncoding = Aeson.toEncoding . fromUserId

newtype RoomId = RoomId {fromRoomId :: Text} deriving (Show, Eq, Ord)

instance Aeson.FromJSON RoomId where
  parseJSON = fmap RoomId . Aeson.parseJSON

instance Aeson.ToJSON RoomId where
  toJSON = Aeson.toJSON . fromRoomId
  toEncoding = Aeson.toEncoding . fromRoomId

newtype UserName = UserName {fromUserName :: NonEmptyText}
  deriving (Show, Eq, Ord)

instance Aeson.FromJSON UserName where
  parseJSON value =
    Aeson.parseJSON value >>= \text ->
      case NonEmptyText.fromText text of
        Nothing -> fail "User name may not be empty"
        Just nonEmptyText -> pure $ UserName nonEmptyText

instance Aeson.ToJSON UserName where
  toJSON = Aeson.toJSON . NonEmptyText.toText . fromUserName
  toEncoding = Aeson.toEncoding . NonEmptyText.toText . fromUserName

data User = User
  { userId :: UserId
  , userName :: UserName
  }
  deriving (Show, Eq)

instance Aeson.ToJSON User where
  toJSON User{..} =
    Aeson.object ["id" Aeson..= userId, "name" Aeson..= userName]
  toEncoding User{..} =
    Aeson.pairs $ mconcat ["id" Aeson..= userId, "name" Aeson..= userName]

data Announcement = Announcement
  { announcedUserName :: UserName
  , announcedRoomId :: RoomId
  }
  deriving (Show, Eq)

instance Aeson.ToJSON Announcement where
  toJSON Announcement{..} =
    Aeson.object
      [ "userName" Aeson..= announcedUserName
      , "roomId" Aeson..= announcedRoomId
      ]
  toEncoding Announcement{..} =
    Aeson.pairs . mconcat $
      [ "userName" Aeson..= announcedUserName
      , "roomId" Aeson..= announcedRoomId
      ]

instance Aeson.FromJSON Announcement where
  parseJSON = Aeson.withObject "Announcement" $ \o ->
    Announcement <$> (o Aeson..: "userName") <*> (o Aeson..: "roomId")

newtype MessageText = MessageText {fromMessageText :: NonEmptyText}
  deriving (Show, Eq)

instance Aeson.ToJSON MessageText where
  toJSON = Aeson.toJSON . NonEmptyText.toText . fromMessageText
  toEncoding = Aeson.toEncoding . NonEmptyText.toText . fromMessageText

instance Aeson.FromJSON MessageText where
  parseJSON value =
    Aeson.parseJSON value >>= \text ->
      case NonEmptyText.fromText text of
        Nothing -> fail "Message text may not be empty"
        Just nonEmptyText -> pure $ MessageText nonEmptyText

data OutputMessage
  = UserLeftMessage User
  | UserJoinedMessage User
  | SentMessage UTCTime User MessageText
  deriving (Show, Eq)

userLeftTag :: Text
userLeftTag = "USER_LEFT"

userJoinedTag :: Text
userJoinedTag = "USER_JOINED"

sentMessageTag :: Text
sentMessageTag = "SENT_MESSAGE"

instance Aeson.ToJSON OutputMessage where
  toJSON outputMessage =
    Aeson.object $ case outputMessage of
      UserLeftMessage user ->
        ["type" Aeson..= userLeftTag, "user" Aeson..= user]
      UserJoinedMessage user ->
        ["type" Aeson..= userJoinedTag, "user" Aeson..= user]
      SentMessage timestamp user message ->
        [ "type" Aeson..= sentMessageTag
        , "user" Aeson..= user
        , "timestamp" Aeson..= timestamp
        , "message" Aeson..= message
        ]
  toEncoding outputMessage =
    Aeson.pairs . mconcat $ case outputMessage of
      UserLeftMessage user ->
        ["type" Aeson..= userLeftTag, "user" Aeson..= user]
      UserJoinedMessage user ->
        ["type" Aeson..= userJoinedTag, "user" Aeson..= user]
      SentMessage timestamp user message ->
        [ "type" Aeson..= sentMessageTag
        , "user" Aeson..= user
        , "timestamp" Aeson..= timestamp
        , "message" Aeson..= message
        ]
