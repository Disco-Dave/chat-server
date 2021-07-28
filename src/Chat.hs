module Chat (
  makeApplication,
  run,
) where

import Chat.Config (Config (..), LoggingConfig (..), WebSocketsConfig (..))
import qualified Chat.Rooms as Rooms
import qualified Chat.WebSockets as ChatWs
import Data.Function ((&))
import qualified Katip
import Network.HTTP.Types (status404)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.WebSockets as WebSockets
import System.IO (stdout)
import UnliftIO (MonadUnliftIO (withRunInIO))
import qualified UnliftIO

makeLogEnv :: LoggingConfig -> IO Katip.LogEnv
makeLogEnv LoggingConfig{..} = do
  fileScribe <-
    Katip.mkFileScribe
      loggingFile
      (Katip.permitItem loggingSeverity)
      loggingVerbosity

  stdoutScribe <-
    Katip.mkHandleScribe
      Katip.ColorIfTerminal
      stdout
      (Katip.permitItem loggingSeverity)
      loggingVerbosity

  Katip.initLogEnv "Chat" loggingEnvironment
    >>= Katip.registerScribe "file" fileScribe Katip.defaultScribeSettings
    >>= Katip.registerScribe "stdout" stdoutScribe Katip.defaultScribeSettings

withLogger :: LoggingConfig -> Katip.KatipContextT IO a -> IO a
withLogger loggingConfig action =
  UnliftIO.bracket (makeLogEnv loggingConfig) Katip.closeScribes $ \logEnv -> do
    Katip.runKatipContextT logEnv () "main" action

emptyApp :: Wai.Application
emptyApp _ respond = respond $ Wai.responseLBS status404 [] mempty

makeApplication :: (MonadUnliftIO m, Katip.KatipContext m) => m Wai.Application
makeApplication = do
  rooms <- Rooms.initialize
  withRunInIO $ \runInIO ->
    pure $
      WaiWs.websocketsOr
        WebSockets.defaultConnectionOptions
        (runInIO . ChatWs.app rooms)
        emptyApp

run :: Config -> IO ()
run config@Config{..} = withLogger configLogging . flip Katip.logExceptionM Katip.ErrorS $ do
  Katip.katipAddContext (Katip.sl "config" config) $
    $(Katip.logTM) Katip.DebugS "Starting application with the supplied configuration"

  app <- makeApplication

  withRunInIO $ \runInIO ->
    let port = webSocketsPort configWebSockets
        beforeMainLoop = runInIO $ $(Katip.logTM) Katip.DebugS ("Now listening on port: " <> Katip.showLS port)
        settings =
          Warp.defaultSettings
            & Warp.setPort port
            & Warp.setBeforeMainLoop beforeMainLoop
     in Warp.runSettings settings app
