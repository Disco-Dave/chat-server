module Chat (
  run,
) where

import Chat.Config (Config (..), LoggingConfig (..))
import qualified Chat.WebSockets as WebSockets
import Control.Exception (bracket)
import qualified Katip
import System.IO (stdout)

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

withLogger :: LoggingConfig -> Katip.KatipContextT IO () -> IO ()
withLogger loggingConfig action =
  bracket (makeLogEnv loggingConfig) Katip.closeScribes $ \logEnv -> do
    Katip.runKatipContextT logEnv () "main" action

run :: Config -> IO ()
run config@Config{..} =
  withLogger configLogging $
    flip Katip.logExceptionM Katip.ErrorS $ do
      Katip.katipAddContext (Katip.sl "config" config) $
        $(Katip.logTM) Katip.DebugS "Starting application with the supplied configuration"
      WebSockets.listen configWebSockets
