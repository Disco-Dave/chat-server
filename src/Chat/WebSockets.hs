module Chat.WebSockets (
  app,
) where

import Chat.Messages (Announcement (..), MessageText (..), User (..), UserId (..))
import qualified Chat.NonEmptyText as NonEmptyText
import Chat.Rooms (Rooms)
import qualified Chat.Rooms as Rooms
import Control.Exception (Exception (fromException))
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Aeson as Aeson
import Data.Functor (void)
import qualified Data.UUID.V4 as UUID
import Data.Void (Void)
import Katip (KatipContext)
import qualified Katip
import qualified Network.WebSockets as WebSockets
import System.Timeout (timeout)
import UnliftIO (MonadUnliftIO (withRunInIO), handleAny, throwIO)
import qualified UnliftIO.Async as Async

getAnnouncement :: KatipContext m => WebSockets.Connection -> m (Maybe Announcement)
getAnnouncement connection = do
  $(Katip.logTM) Katip.DebugS "Attempting to get announcement message"
  message <- liftIO $ timeout 300_000_000 (WebSockets.receiveData connection)
  case fmap Aeson.decode message of
    Nothing -> do
      $(Katip.logTM) Katip.WarningS "Request to get announcement message timed out"
      pure Nothing
    Just Nothing -> do
      $(Katip.logTM) Katip.WarningS "Announcement payload was unrecognized"
      pure Nothing
    Just announcement -> do
      $(Katip.logTM) Katip.DebugS "Announcement successfully parsed"
      pure announcement

handleUserInput :: KatipContext m => WebSockets.Connection -> Rooms.Sender m -> m Void
handleUserInput connection send = forever $ do
  message <- liftIO $ WebSockets.receiveData connection
  Katip.katipAddContext (Katip.sl "message" message) $ do
    $(Katip.logTM) Katip.DebugS "Received message"
    case NonEmptyText.fromText message of
      Nothing -> do
        $(Katip.logTM) Katip.WarningS "Message is empty"
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

startUserThreads :: (MonadUnliftIO m, KatipContext m) => WebSockets.Connection -> Rooms.Room m -> m ()
startUserThreads connection (send, receive) = do
  $(Katip.logTM) Katip.DebugS "Starting user input and room event handler threads"
  void $
    Async.race
      (handleUserInput connection send)
      (handleRoomEvents connection receive)

handleAnnouncement :: (MonadUnliftIO m, KatipContext m) => Rooms -> WebSockets.Connection -> Announcement -> m ()
handleAnnouncement rooms connection Announcement{..} = do
  userId <- liftIO $ UserId <$> UUID.nextRandom
  liftIO $ WebSockets.sendTextData connection (Aeson.encode userId)
  let user = User userId announcedUserName
  Rooms.withRoom rooms announcedRoomId user (startUserThreads connection)

logException :: (UnliftIO.MonadUnliftIO m, Katip.KatipContext m) => m a -> m a
logException =
  let handler e = do
        case fromException e of
          Just (WebSockets.CloseRequest _ _) -> pure ()
          Just WebSockets.ConnectionClosed -> pure ()
          _ ->
            let msg = "An exception has occurred: " <> Katip.showLS e
             in $(Katip.logTM) Katip.ErrorS msg
        throwIO e
   in handleAny handler

app :: (KatipContext m, MonadUnliftIO m) => Rooms -> WebSockets.PendingConnection -> m ()
app rooms pendingConnection = do
  context <- Katip.sl "connectionId" <$> liftIO UUID.nextRandom
  logException . Katip.katipAddContext context . Katip.katipAddNamespace "web-sockets" $ do
    connection <- liftIO $ WebSockets.acceptRequest pendingConnection
    withRunInIO $ \runInIO ->
      WebSockets.withPingThread connection 30 (pure ()) $
        runInIO $ do
          maybeAnnouncement <- getAnnouncement connection
          Katip.katipAddContext (Katip.sl "announcement" maybeAnnouncement) $
            $(Katip.logTM) Katip.DebugS "Got announcement"
          case maybeAnnouncement of
            Nothing -> do
              $(Katip.logTM) Katip.WarningS "Announcement wasn't obtained, closing connection."
              pure ()
            Just announcement ->
              handleAnnouncement rooms connection announcement
