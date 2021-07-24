module Chat.Rooms (
  Rooms,
  Sender,
  Receiver,
  initialize,
  withRoom,
) where

import Chat.Messages (MessageText, OutputMessage (..), RoomId, User)
import qualified Control.Concurrent.STM as STM
import Control.Exception (bracket)
import Control.Monad (when, (<=<))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Time as Time
import Katip (KatipContext)
import qualified Katip
import Numeric.Natural (Natural)

data RoomState = RoomState
  { roomConnectionCount :: STM.TVar Natural
  , roomChan :: STM.TChan OutputMessage
  }

newtype Rooms = Rooms (STM.TVar (Map RoomId RoomState))

type Sender m = MessageText -> m ()
type Receiver m = m OutputMessage

initialize :: KatipContext m => m Rooms
initialize = do
  $(Katip.logTM) Katip.DebugS "Initializing the room state"
  liftIO $ Rooms <$> STM.newTVarIO Map.empty

acquire :: KatipContext m => Rooms -> RoomId -> User -> m RoomState
acquire (Rooms rooms) roomId user = do
  (roomState, newCount) <- liftIO . STM.atomically $ do
    roomMap <- STM.readTVar rooms
    roomState@RoomState{..} <- case Map.lookup roomId roomMap of
      Just roomState -> pure roomState
      Nothing -> do
        roomState <- RoomState <$> STM.newTVar 0 <*> STM.newBroadcastTChan
        STM.modifyTVar' rooms (Map.insert roomId roomState)
        pure roomState
    STM.modifyTVar' roomConnectionCount succ
    STM.writeTChan roomChan (UserJoinedMessage user)
    (roomState,) <$> STM.readTVar roomConnectionCount
  Katip.katipAddContext (Katip.sl "connections" newCount) $ do
    $(Katip.logTM) Katip.DebugS "RoomState acquired"
    pure roomState

release :: KatipContext m => Rooms -> RoomId -> User -> RoomState -> m ()
release (Rooms rooms) roomId user RoomState{..} = do
  newCount <- liftIO . STM.atomically $ do
    STM.writeTChan roomChan (UserLeftMessage user)
    STM.modifyTVar' roomConnectionCount pred
    remainingCount <- STM.readTVar roomConnectionCount
    when (remainingCount <= 0) $
      STM.modifyTVar' rooms (Map.delete roomId)
    pure remainingCount
  Katip.katipAddContext (Katip.sl "connections" newCount) $ do
    $(Katip.logTM) Katip.DebugS "RoomState released"

createSenderAndReceiver :: (KatipContext m) => User -> RoomState -> m (Sender m, Receiver m)
createSenderAndReceiver user RoomState{roomChan} = do
  userChan <- liftIO $ STM.atomically (STM.dupTChan roomChan)
  let receiver = liftIO . STM.atomically $ STM.readTChan userChan
  let sender msg = liftIO $ do
        timestamp <- Time.getCurrentTime
        let sentMessage = SentMessage timestamp user msg
        STM.atomically $ STM.writeTChan userChan sentMessage
  $(Katip.logTM) Katip.DebugS "Created Sender and Receiver"
  pure (sender, receiver)

withRoom :: (MonadUnliftIO m, KatipContext m) => Rooms -> RoomId -> User -> ((Sender m, Receiver m) -> m ()) -> m ()
withRoom rooms roomId user action =
  let withKatip = Katip.katipAddNamespace "room" . Katip.katipAddContext (Katip.sl "roomId" roomId <> Katip.sl "user" user)
   in withRunInIO $ \runInIO ->
        bracket
          (runInIO . withKatip $ acquire rooms roomId user)
          (runInIO . withKatip . release rooms roomId user)
          (runInIO . (action <=< createSenderAndReceiver user))
