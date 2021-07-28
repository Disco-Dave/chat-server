{-# LANGUAGE QuasiQuotes #-}

module ChatSpec (spec) where

import qualified Chat
import Chat.Messages (
  MessageText (fromMessageText),
  OutputMessage (..),
  User (User),
  UserId,
  UserName (UserName),
 )
import qualified Chat.NonEmptyText as NonEmptyText
import qualified Data.Aeson as Aeson
import Data.Aeson.QQ (aesonQQ)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Katip
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WebSockets
import Test.Hspec (
  Spec,
  around,
  expectationFailure,
  it,
  shouldBe,
  shouldNotBe,
  shouldSatisfy,
 )

withServer :: (Warp.Port -> IO a) -> IO a
withServer action = do
  env <- Katip.initLogEnv "Chat" "testing"
  let app = Katip.runKatipContextT env () "chat-spec" Chat.makeApplication
  Warp.withApplication app action

runClient :: Warp.Port -> WebSockets.ClientApp a -> IO a
runClient port =
  WebSockets.runClient "127.0.0.1" port "/"

spec :: Spec
spec =
  around withServer $ do
    it "responds with user id after connecting" $ \port -> do
      userId1 <- runClient port $ \conn -> do
        WebSockets.sendTextData conn (Aeson.encode [aesonQQ| { userName: "test-user", roomId: "test-room" } |])
        fromMaybe "" . Aeson.decode @Text <$> WebSockets.receiveData conn

      userId1 `shouldSatisfy` (not . Text.null)

      userId2 <- runClient port $ \conn -> do
        WebSockets.sendTextData conn (Aeson.encode [aesonQQ| { userName: "test-other-user", roomId: "test-room" } |])
        fromMaybe "" . Aeson.decode @Text <$> WebSockets.receiveData conn

      userId2 `shouldSatisfy` (not . Text.null)

      userId1 `shouldNotBe` userId2

    it "sends message when a user joins or leaves the room" $ \port -> do
      (userId, joinMessage, leaveMessage) <- runClient port $ \conn -> do
        WebSockets.sendTextData conn (Aeson.encode [aesonQQ| { userName: "test-user", roomId: "test-room" } |])
        _ <- WebSockets.receiveData @Text conn

        userId <- runClient port $ \conn2 -> do
          WebSockets.sendTextData conn2 (Aeson.encode [aesonQQ| { userName: "someone-else", roomId: "test-room" } |])
          fromJust . Aeson.decode @UserId <$> WebSockets.receiveData conn2

        joinMessage <- Aeson.decode @OutputMessage <$> WebSockets.receiveData conn
        leaveMessage <- Aeson.decode @OutputMessage <$> WebSockets.receiveData conn
        pure (userId, joinMessage, leaveMessage)

      let expectedUser = User userId (UserName (fromJust $ NonEmptyText.fromText "someone-else"))
      let expectedJoinedMessage = UserJoinedMessage expectedUser
      let expectedLeftMessage = UserLeftMessage expectedUser

      joinMessage `shouldBe` Just expectedJoinedMessage
      leaveMessage `shouldBe` Just expectedLeftMessage

    it "sends message when a user sends a message to the room" $ \port -> do
      (userId, outputMessage) <- runClient port $ \conn -> do
        WebSockets.sendTextData conn (Aeson.encode [aesonQQ| { userName: "test-user", roomId: "test-room" } |])
        _ <- WebSockets.receiveData @Text conn

        userId <- runClient port $ \conn2 -> do
          WebSockets.sendTextData conn2 (Aeson.encode [aesonQQ| { userName: "someone-else", roomId: "test-room" } |])
          userId <- fromJust . Aeson.decode @UserId <$> WebSockets.receiveData conn2
          WebSockets.sendTextData conn2 (Aeson.encode @Text "This is a sample message.")
          pure userId

        _ <- WebSockets.receiveData @Text conn
        sentMessage <- Aeson.decode @OutputMessage <$> WebSockets.receiveData conn

        pure (userId, sentMessage)

      let expectedUser = User userId (UserName (fromJust $ NonEmptyText.fromText "someone-else"))
      let expectedMessage = "\"This is a sample message.\"" :: Text

      case outputMessage of
        Just (SentMessage _ actualUser actualMessage) -> do
          actualUser `shouldBe` expectedUser
          NonEmptyText.toText (fromMessageText actualMessage) `shouldBe` expectedMessage
        _ -> expectationFailure "Unrecognized responses"
