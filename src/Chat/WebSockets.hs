module Chat.WebSockets (
  listen,
) where

import Chat.Config (WebSocketsConfig (..))
import Chat.Messages (Announcement (..), MessageText (..), User (..), UserId (..))
import qualified Chat.NonEmptyText as NonEmptyText
import Chat.Rooms (Rooms)
import qualified Chat.Rooms as Rooms
import qualified Control.Concurrent.Async as Async
import Control.Monad (forever, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import qualified Data.Aeson as Aeson
import Data.Maybe (isNothing)
import qualified Data.UUID.V4 as UUID
import Data.Void (Void)
import Katip (KatipContext)
import qualified Katip
import qualified Network.WebSockets as WebSockets
import System.Timeout (timeout)

getAnnouncement :: KatipContext m => WebSockets.Connection -> m (Maybe Announcement)
getAnnouncement connection = do
  $(Katip.logTM) Katip.DebugS "Attempting to get announcement message"
  message <- liftIO $ timeout 300_000_000 (WebSockets.receiveData connection)
  when (isNothing message) $
    $(Katip.logTM) Katip.DebugS "Request to get announcement message timed out"
  pure $ message >>= Aeson.decode

handleUserInput :: KatipContext m => WebSockets.Connection -> Rooms.Sender m -> m Void
handleUserInput connection send = forever $ do
  message <- liftIO $ WebSockets.receiveData connection
  Katip.katipAddContext (Katip.sl "message" message) $ do
    $(Katip.logTM) Katip.DebugS "Received message"
    case NonEmptyText.fromText message of
      Nothing -> do
        $(Katip.logTM) Katip.DebugS "Message is empty"
        pure ()
      Just nonEmptyText -> do
        $(Katip.logTM) Katip.DebugS "Sending message to room"
        send $ MessageText nonEmptyText

handleRoomEvents :: KatipContext m => WebSockets.Connection -> Rooms.Receiver m -> m Void
handleRoomEvents connection receive = forever $ do
  event <- receive
  Katip.katipAddContext (Katip.sl "event" event) $ do
    $(Katip.logTM) Katip.DebugS "Received room event"
    liftIO $ WebSockets.sendTextData connection (Aeson.encode event)

startUserThreads :: (MonadUnliftIO m, KatipContext m) => WebSockets.Connection -> (Rooms.Sender m, Rooms.Receiver m) -> m ()
startUserThreads connection (send, receive) = do
  $(Katip.logTM) Katip.DebugS "Starting user input and room event handler threads"
  _ <- withRunInIO $ \runInIO -> do
    Async.race
      (runInIO $ handleUserInput connection send)
      (runInIO $ handleRoomEvents connection receive)
  pure ()

serverApp :: (KatipContext m, MonadUnliftIO m) => Rooms -> WebSockets.PendingConnection -> m ()
serverApp rooms pendingConnection = do
  connection <- liftIO $ WebSockets.acceptRequest pendingConnection
  withRunInIO $ \runInIO ->
    WebSockets.withPingThread connection 30 (pure ()) $
      runInIO $ do
        announcement <- getAnnouncement connection
        Katip.katipAddContext (Katip.sl "announcement" announcement) $
          $(Katip.logTM) Katip.DebugS "Got announcement"
        case announcement of
          Nothing -> do
            $(Katip.logTM) Katip.DebugS "Announcement wasn't obtained, closing connection."
            pure ()
          Just Announcement{..} -> do
            userId <- liftIO $ UserId <$> UUID.nextRandom
            liftIO $ WebSockets.sendTextData connection (Aeson.encode userId)
            let user = User userId announcedUserName
            Rooms.withRoom rooms announcedRoomId user (startUserThreads connection)

listen :: (KatipContext m, MonadUnliftIO m) => WebSocketsConfig -> m ()
listen WebSocketsConfig{..} = do
  let msg =
        "Listening for websockets on host "
          <> Katip.showLS webSocketsHost
          <> " and port "
          <> Katip.showLS webSocketsPort
   in $(Katip.logTM) Katip.InfoS msg
  rooms <- Rooms.initialize
  withRunInIO $ \runInIO ->
    WebSockets.runServer
      webSocketsHost
      webSocketsPort
      (runInIO . serverApp rooms)
