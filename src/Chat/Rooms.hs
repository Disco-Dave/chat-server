module Chat.Rooms (
  Rooms,
  Sender,
  Receiver,
  Room,
  initialize,
  withRoom,
) where

import Chat.Messages (MessageText, OutputMessage (..), RoomId, User)
import qualified Control.Concurrent.STM as STM
import Control.Monad (when, (<=<))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Time as Time
import Katip (KatipContext)
import qualified Katip
import Numeric.Natural (Natural)
import UnliftIO (MonadUnliftIO, bracket)

data RoomState = RoomState
  { roomConnectionCount :: STM.TVar Natural
  , roomChan :: STM.TChan OutputMessage
  }

newtype Rooms = Rooms (STM.TVar (Map RoomId RoomState))

type Sender m = MessageText -> m ()
type Receiver m = m OutputMessage

type Room m = (Sender m, Receiver m)

initialize :: KatipContext m => m Rooms
initialize = do
  $(Katip.logTM) Katip.DebugS "Initializing the room state"
  liftIO $ Rooms <$> STM.newTVarIO Map.empty

getRoom :: Rooms -> RoomId -> STM.STM RoomState
getRoom (Rooms rooms) roomId = do
  roomMap <- STM.readTVar rooms
  case Map.lookup roomId roomMap of
    Just roomState -> pure roomState
    Nothing -> do
      roomState <- RoomState <$> STM.newTVar 0 <*> STM.newBroadcastTChan
      STM.modifyTVar' rooms (Map.insert roomId roomState)
      pure roomState

addUser :: RoomState -> User -> STM.STM ()
addUser RoomState{..} user = do
  STM.modifyTVar' roomConnectionCount succ
  STM.writeTChan roomChan (UserJoinedMessage user)

removeUser :: RoomState -> User -> STM.STM ()
removeUser RoomState{..} user = do
  STM.writeTChan roomChan (UserLeftMessage user)
  STM.modifyTVar' roomConnectionCount pred

acquire :: KatipContext m => Rooms -> RoomId -> User -> m RoomState
acquire rooms roomId user = do
  (roomState, newCount) <- liftIO . STM.atomically $ do
    roomState@RoomState{roomConnectionCount} <- getRoom rooms roomId
    addUser roomState user
    (roomState,) <$> STM.readTVar roomConnectionCount

  Katip.katipAddContext (Katip.sl "connections" newCount) $ do
    $(Katip.logTM) Katip.DebugS "Room state acquired"

  pure roomState

release :: KatipContext m => Rooms -> RoomId -> User -> RoomState -> m ()
release (Rooms rooms) roomId user roomState@RoomState{roomConnectionCount} = do
  newCount <- liftIO . STM.atomically $ do
    removeUser roomState user

    remainingCount <- STM.readTVar roomConnectionCount
    when (remainingCount <= 0) $
      STM.modifyTVar' rooms (Map.delete roomId)

    pure remainingCount

  Katip.katipAddContext (Katip.sl "connections" newCount) $ do
    $(Katip.logTM) Katip.DebugS "Room state released"

createRoom :: KatipContext m => User -> RoomState -> m (Room m)
createRoom user RoomState{roomChan} = do
  userChan <- liftIO $ STM.atomically (STM.dupTChan roomChan)

  let receiver = liftIO . STM.atomically $ STM.readTChan userChan
  let sender msg = liftIO $ do
        timestamp <- Time.getCurrentTime
        let sentMessage = SentMessage timestamp user msg
        STM.atomically $ STM.writeTChan userChan sentMessage

  $(Katip.logTM) Katip.DebugS "Created sender and receiver for the room"

  pure (sender, receiver)

withRoom :: (MonadUnliftIO m, KatipContext m) => Rooms -> RoomId -> User -> (Room m -> m ()) -> m ()
withRoom rooms roomId user action =
  let withNamespace = Katip.katipAddNamespace "room"
      withContext = Katip.katipAddContext $ Katip.sl "roomId" roomId <> Katip.sl "user" user
   in withContext . withNamespace $
        bracket
          (acquire rooms roomId user)
          (release rooms roomId user)
          (action <=< createRoom user)
